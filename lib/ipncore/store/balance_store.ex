defmodule BalanceStore do
  @table "balance"
  alias Exqlite.Sqlite3NIF

  @token Application.compile_env(:ipncore, :token)

  use Store.Sqlite,
    base: :balance,
    table: @table,
    mod: Ippan.Balance,
    create: ~c"
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT NOT NULL,
      token VARCHAR(20) NOT NULL,
      amount BIGINT DEFAULT 0,
      locked BIGINT DEFAULT 0,
      created_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL,
      PRIMARY KEY (id, token)
    ) WITHOUT ROWID;",
    stmt: %{
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6)",
      replace: ~c"REPLACE INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6)",
      balance: ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND token = ?2 AND amount >= ?3",
      lookup: ~c"SELECT * FROM #{@table} WHERE id = ?1 AND token = ?2",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND token = ?2",
      delete: ~c"DELETE FROM #{@table} WHERE id = ?1 AND token = ?2",
      send:
        ~c"UPDATE #{@table} SET amount = amount - ?3, updated_at = ?4 WHERE id = ?1 AND token = ?2 AND amount >= ?3",
      income: ~c"INSERT INTO #{@table} (id,token,amount,created_at,updated_at)
      VALUES(?1, ?2, ?3, ?4, ?4) ON CONFLICT (id, token)
      DO UPDATE SET amount = amount + ?3, updated_at = ?4
      WHERE id = ?1 AND token = ?2",
      lock:
        ~c"UPDATE #{@table} SET amount = amount - ?3, locked = locked + ?3 WHERE id = ?1 AND token =?2 AND amount >= ?3",
      unlock:
        ~c"UPDATE #{@table} SET amount = amount + ?3, locked = locked - ?3 WHERE id = ?1 AND token =?2 AND locked >= ?3"
    }

  # def count_last_activity(timestamp) do
  #   call({:execute_fetch, :count_last_activity, [@token, timestamp]})
  # end

  # def last_activity(timestamp) do
  #   call({:execute_fetch, :last_activity, [@token, timestamp]})
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

    call({:call, fun})
  end

  @spec burn(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          1 | 0
  def burn(from_id, token_id, amount, timestamp) do
    call({:execute_changes, :send, [from_id, token_id, amount, timestamp]})
  end

  @spec send(String.t(), String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send(from_id, to_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      case Sqlite3NIF.bind_step_changes(conn, stmt.send, [
             from_id,
             token_id,
             amount,
             timestamp
           ]) do
        1 ->
          Sqlite3NIF.bind_and_step(conn, stmt.income, [
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

    call({:call, fun})
  end

  def deferred(id, type, from, to, token, amount, timestamp, hash, round) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      case Sqlite3NIF.bind_step_changes(conn, stmt.send, [
             from,
             token,
             amount,
             timestamp
           ]) do
        1 ->
          case Sqlite3NIF.bind_and_step(conn, stmt.deferred, [
                 id,
                 type,
                 from,
                 token,
                 to,
                 amount,
                 timestamp,
                 hash,
                 round
               ]) do
            {:row, [ret_from, ret_token, ret_amount]} = r ->
              IO.inspect(r)

              if ret_from != from do
                Sqlite3NIF.bind_and_step(conn, stmt.income, [
                  ret_from,
                  ret_token,
                  ret_amount,
                  timestamp
                ])
              end

              {:reply, :ok, state}

            _ ->
              # rollback
              Sqlite3NIF.bind_and_step(conn, stmt.income, [
                from,
                token,
                amount,
                timestamp
              ])

              {:reply, 0, state}
          end

        _ ->
          {:reply, :error, state}
      end
    end

    call({:call, fun})
  end

  @spec send_fees(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send_fees(from_id, validator_owner, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      case Sqlite3NIF.bind_step_changes(conn, stmt.send, [
             from_id,
             @token,
             amount,
             timestamp
           ]) do
        1 ->
          Sqlite3NIF.bind_and_step(conn, stmt.income, [
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

    call({:call, fun})
  end

  def transaction(from_id, to_id, token, amount, validator_owner, fees, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = stmt.send
      income_stmt = stmt.income

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

    call({:call, fun})
  end

  @spec lock(binary, String.t(), non_neg_integer()) :: boolean()
  def lock(id, token, amount) do
    call({:execute_changes, :lock, [id, token, amount]})
  end

  @spec unlock(binary, String.t(), non_neg_integer()) :: boolean()
  def unlock(id, token, amount) do
    call({:execute_changes, :unlock, [id, token, amount]})
  end

  def balance(address, token, amount) do
    case call({:execute_step, :balance, [address, token, amount]}) do
      {:row, [1]} ->
        :ok

      _ ->
        :error
    end
  end

  def balance2(address, token, amount, token2, amount2) do
    case call({:execute_step, :balance, [address, token, amount]}) do
      {:row, [1]} ->
        case call({:execute_step, :balance, [address, token2, amount2]}) do
          {:row, [1]} ->
            true

          _ ->
            false
        end

      _ ->
        false
    end
  end

  def receive(conn, recv_stmt, to_id, token, amount, timestamp)
      when is_reference(conn) do
    Sqlite3NIF.bind_and_step(conn, recv_stmt, [to_id, token, amount, timestamp])
  end
end
