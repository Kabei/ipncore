defmodule BalanceStore do
  @table "balance"
  alias Exqlite.Sqlite3NIF

  @token Application.compile_env(:ipncore, :token)
  @maximum_amount 9_223_372_036_854_775_807 |> to_string()

  @args %{
    "table" => @table,
    "maximum_amount" => @maximum_amount
  }

  use Store.Sqlite2,
    base: :balance,
    table: @table,
    mod: Ippan.Balance,
    create: SQL.readFile!("lib/sql/balance.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/balance.stmt.sql", @args)

  # def count_last_activity(timestamp) do
  #   call({:fetch, :count_last_activity, [@token, timestamp]})
  # end

  # def last_activity(timestamp) do
  #   call({:fetch, :last_activity, [@token, timestamp]})
  # end

  @spec income(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def income(account_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, :income)

      case Sqlite3NIF.bind_step_changes(conn, send_stmt, [
             account_id,
             token_id,
             amount,
             timestamp
           ]) do
        1 ->
          {:reply, :ok, state}

        _ ->
          {:reply, :error, state}
      end
    end

    call({:run, fun})
  end

  @spec burn(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          1 | 0
  def burn(from_id, token_id, amount, timestamp) do
    call({:changes, :send, [from_id, token_id, amount, timestamp]})
  end

  @spec send(String.t(), String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send(from_id, to_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      case Sqlite3NIF.bind_step_changes(conn, Map.get(stmt, "send"), [
             from_id,
             token_id,
             amount,
             timestamp
           ]) do
        1 ->
          Sqlite3NIF.bind_and_step(conn, Map.get(stmt, "income"), [
            to_id,
            token_id,
            amount,
            timestamp
          ])

          {:reply, :ok, state}

        _ ->
          {:reply, :error, state}
      end
    end

    call({:run, fun})
  end

  @spec send_fees(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send_fees(from_id, validator_owner, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      case Sqlite3NIF.bind_step_changes(conn, Map.get(stmt, "send"), [
             from_id,
             @token,
             amount,
             timestamp
           ]) do
        1 ->
          Sqlite3NIF.bind_and_step(conn, Map.get(stmt, "income"), [
            validator_owner,
            @token,
            amount,
            timestamp
          ])

          {:reply, :ok, state}

        _ ->
          {:reply, :error, state}
      end
    end

    call({:run, fun})
  end

  def transaction(from_id, to_id, token, amount, validator_owner, fees, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, "send")
      income_stmt = Map.get(stmt, "income")

      ret =
        if token == @token do
          total = amount + fees

          case Sqlite3NIF.bind_step_changes(conn, send_stmt, [
                 from_id,
                 token,
                 total,
                 timestamp
               ]) do
            1 ->
              :ok

            _ ->
              :error
          end
        else
          case Sqlite3NIF.bind_step_changes(conn, send_stmt, [
                 from_id,
                 token,
                 amount,
                 timestamp
               ]) do
            1 ->
              case Sqlite3NIF.bind_step_changes(conn, send_stmt, [
                     from_id,
                     @token,
                     fees,
                     timestamp
                   ]) do
                1 ->
                  :ok

                _ ->
                  # rollback
                  Sqlite3NIF.bind_and_step(conn, income_stmt, [
                    from_id,
                    token,
                    amount,
                    timestamp
                  ])

                  :error
              end

            _ ->
              :error
          end
        end

      if ret == :ok do
        Sqlite3NIF.bind_and_step(conn, income_stmt, [to_id, token, amount, timestamp])

        Sqlite3NIF.bind_and_step(conn, income_stmt, [
          validator_owner,
          @token,
          fees,
          timestamp
        ])

        {:reply, :ok, state}
      else
        {:reply, :error, state}
      end
    end

    call({:run, fun})
  end

  @spec lock(binary, String.t(), non_neg_integer()) :: integer()
  def lock(id, token, amount) do
    call({:changes, :lock, [id, token, amount]})
  end

  @spec unlock(binary, String.t(), non_neg_integer()) :: integer()
  def unlock(id, token, amount) do
    call({:changes, :unlock, [id, token, amount]})
  end

  def balance(_address, _token, 0), do: :ok

  def balance(address, token, amount) do
    case call({:step, :balance, [address, token, amount]}) do
      {:row, [1]} ->
        :ok

      _ ->
        :error
    end
  end

  # def balance2(address, token, amount, token2, amount2) do
  #   case call({:step, :balance, [address, token, amount]}) do
  #     {:row, [1]} ->
  #       case call({:step, :balance, [address, token2, amount2]}) do
  #         {:row, [1]} ->
  #           true

  #         _ ->
  #           false
  #       end

  #     _ ->
  #       false
  #   end
  # end

  def receive(conn, recv_stmt, to_id, token, amount, timestamp)
      when is_reference(conn) do
    Sqlite3NIF.bind_and_step(conn, recv_stmt, [to_id, token, amount, timestamp])
  end
end
