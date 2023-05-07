defmodule BalanceStore do
  @table "balance"
  alias Exqlite.Sqlite3NIF

  @token Default.token()

  use Store.Sqlite,
    base: :balance,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT NOT NULL,
      token VARCHAR(20) NOT NULL,
      amount UNSIGNED BIGINT DEFAULT 0,
      deferred UNSIGNED BIGINT DEFAULT 0,
      locked UNSIGNED BIGINT DEFAULT 0,
      tx_count UNSIGNED BIGINT DEFAULT 0,
      created_at UNSIGNED BIGINT NOT NULL,
      updated_at UNSIGNED BIGINT NOT NULL,
      PRIMARY KEY (id, token)
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?1 AND token = ?2",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?1 AND token = ?2",
      delete: "DELETE FROM #{@table} WHERE id = ?1 AND token = ?2",
      send:
        "UPDATE #{@table} SET amount = amount - ?3, tx_count = tx_count + 1, updated_at = ?4 WHERE id = ?1 AND token = ?2 AND amount > ?3",
      income: """
      INSERT INTO #{@table} (id,token,amount,tx_count,created_at,updated_at)
      VALUES(?1, ?2, ?3, 1, ?4, ?4) ON CONFLICT (id, token)
      DO UPDATE SET amount = amount + ?3, tx_count = tx_count + 1, updated_at = ?4
      WHERE id = ?1 AND token = ?2
      """,
      lock:
        "UPDATE #{@table} SET amount = amount - ?3, locked = locked + ?3 WHERE id = ?1 AND token =?2 AND amount >= ?3",
      unlock:
        "UPDATE #{@table} SET amount = amount + ?3, locked = locked - ?3 WHERE id = ?1 AND token =?2 AND locked >= ?3"
      # deferred:
      #   "UPDATE #{@table} SET deferred = deferred + ?3, amount = amount - ?3 WHERE id = ?1 AND token = ?2 AND amount >= ?3",
      # return_deferred:
      #   "UPDATE #{@table} SET deferred = deferred - ?3, amount = amount + ?3 WHERE id = ?1 AND token = ?2 AND deferred >= ?3",
      # delete_all_deferred: "UPDATE #{@table} SET deferred = 0"
    }

  @spec income(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def income(account_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, :income)

      Sqlite3NIF.bind_and_step(conn, send_stmt, [
        account_id,
        token_id,
        amount,
        timestamp
      ])

      case Sqlite3NIF.changes(conn) do
        {:ok, 1} ->
          {:reply, :ok, state}

        _ ->
          {:reply, :error, state}
      end
    end

    call(@base, {:call, fun})
  end

  @spec burn(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def burn(from_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, :send)

      Sqlite3NIF.bind_and_step(conn, send_stmt, [from_id, token_id, amount, timestamp])

      case Sqlite3NIF.changes(conn) do
        {:ok, 1} ->
          {:reply, :ok, state}

        _ ->
          {:reply, :error, state}
      end
    end

    call(@base, {:call, fun})
  end

  @spec send(String.t(), String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send(from_id, to_id, token_id, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = stmt.send
      income_stmt = stmt.income

      Sqlite3NIF.bind_and_step(conn, send_stmt, [from_id, token_id, amount, timestamp])

      case Sqlite3NIF.changes(conn) do
        {:ok, 1} ->
          Sqlite3NIF.bind_and_step(conn, income_stmt, [
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

    call(@base, {:call, fun})
  end

  @spec send_fees(String.t(), String.t(), non_neg_integer(), non_neg_integer()) ::
          :ok | :error
  def send_fees(from_id, validator_owner, amount, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, :send)
      income_stmt = Map.get(stmt, :income)

      Sqlite3NIF.bind_and_step(conn, send_stmt, [from_id, @token, amount, timestamp])

      case Sqlite3NIF.changes(conn) do
        {:ok, 1} ->
          Sqlite3NIF.bind_and_step(conn, income_stmt, [
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

    call(@base, {:call, fun})
  end

  @spec transaction(
          String.t(),
          String.t(),
          String.t(),
          non_neg_integer(),
          integer(),
          non_neg_integer(),
          non_neg_integer()
        ) ::
          :ok | :error
  def transaction(from_id, to_id, token, amount, validator_owner, fees, timestamp) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      send_stmt = Map.get(stmt, :send)
      income_stmt = Map.get(stmt, :income)

      ret =
        if token == @token do
          total = amount + fees
          Sqlite3NIF.bind_and_step(conn, send_stmt, [from_id, token, total, timestamp])

          case Sqlite3NIF.changes(conn) do
            {:ok, 1} ->
              :ok

            _ ->
              :error
          end
        else
          Sqlite3NIF.bind_and_step(conn, send_stmt, [from_id, token, amount, timestamp])

          case Sqlite3NIF.changes(conn) do
            {:ok, 1} ->
              Sqlite3NIF.bind_and_step(conn, send_stmt, [
                from_id,
                @token,
                fees,
                timestamp
              ])

              case Sqlite3NIF.changes(conn) do
                {:ok, 1} ->
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
          amount,
          timestamp
        ])

        {:reply, :ok, state}
      else
        {:reply, :error, state}
      end
    end

    call(@base, {:call, fun})
  end

  @spec lock(binary, String.t(), non_neg_integer()) :: boolean()
  def lock(id, token, amount) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      stmt = Map.get(stmt, :lock)

      Sqlite3NIF.bind_and_step(conn, stmt, [id, token, amount])
      {:reply, changes(conn) == 1, state}
    end

    call(@base, {:call, fun})
  end

  @spec unlock(binary, String.t(), non_neg_integer()) :: boolean()
  def unlock(id, token, amount) do
    fun = fn %{conn: conn, stmt: stmt} = state ->
      stmt = Map.get(stmt, :unlock)

      Sqlite3NIF.bind_and_step(conn, stmt, [id, token, amount])
      {:reply, changes(conn) == 1, state}
    end

    call(@base, {:call, fun})
  end

  def receive(conn, recv_stmt, to_id, token, amount, timestamp)
      when is_reference(conn) do
    Sqlite3NIF.bind_and_step(conn, recv_stmt, [to_id, token, amount, timestamp])
  end
end
