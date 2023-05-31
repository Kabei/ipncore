defmodule BalanceStore do
  @table "balance"
  @table_df "balance_df"
  alias Exqlite.Sqlite3NIF

  @token Default.token()

  use Store.Sqlite,
    base: :balance,
    # pool: :bal_pool,
    table: @table,
    mod: Ippan.Balance,
    # deferred UNSIGNED BIGINT DEFAULT 0,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT NOT NULL,
      token VARCHAR(20) NOT NULL,
      amount UNSIGNED BIGINT DEFAULT 0,
      locked UNSIGNED BIGINT DEFAULT 0,
      tx_count UNSIGNED BIGINT DEFAULT 0,
      created_at UNSIGNED BIGINT NOT NULL,
      updated_at UNSIGNED BIGINT NOT NULL,
      PRIMARY KEY (id, token)
    ) WITHOUT ROWID;", "
    CREATE TABLE IF NOT EXISTS #{@table_df}(
      id BLOB NOT NULL,
      type TINYINT NOT NULL,
      `from` BLOB NOT NULL,
      token VARCHAR(20) NOT NULL,
      `to` BLOB NOT NULL,
      amount BIGINT DEFAULT 0,
      created_at UNSIGNED BIGINT NOT NULL,
      hash BLOB NOT NULL,
      round BIGINT NOT NULL,
      PRIMARY KEY (id, type)
    ) WITHOUT ROWID;
    "],
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7)",
      replace: "REPLACE INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7)",
      balance: "SELECT amount FROM #{@table} WHERE id = ?1 AND token = ?2",
      lookup: "SELECT * FROM #{@table} WHERE id = ?1 AND token = ?2",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?1 AND token = ?2",
      delete: "DELETE FROM #{@table} WHERE id = ?1 AND token = ?2",
      send:
        "UPDATE #{@table} SET amount = amount - ?3, tx_count = tx_count + 1, updated_at = ?4 WHERE id = ?1 AND token = ?2 AND amount >= ?3",
      deferred:
        "INSERT INTO #{@table_df} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9) ON CONFLICT (id, type)
        DO UPDATE SET amount = amount + ?6, created_at=?7, hash=?8, round=?9
        WHERE created_at > ?7 OR created_at=?7 AND hash > ?8 RETURNING `from`, token, amount",
      income: "INSERT INTO #{@table} (id,token,amount,tx_count,created_at,updated_at)
      VALUES(?1, ?2, ?3, 1, ?4, ?4) ON CONFLICT (id, token)
      DO UPDATE SET amount = amount + ?3, tx_count = tx_count + 1, updated_at = ?4
      WHERE id = ?1 AND token = ?2",
      lock:
        "UPDATE #{@table} SET amount = amount - ?3, locked = locked + ?3 WHERE id = ?1 AND token =?2 AND amount >= ?3",
      unlock:
        "UPDATE #{@table} SET amount = amount + ?3, locked = locked - ?3 WHERE id = ?1 AND token =?2 AND locked >= ?3",
      move:
        "INSERT INTO #{@table} (id,token,amount,created_at,updated_at)
        SELECT `to`, token, amount, created_at, created_at FROM #{@table_df} WHERE round=?1
        ON CONFLICT (id,token) DO UPDATE SET amount = amount + EXCLUDED.amount, updated_at=EXCLUDED.updated_at",
      delete_deferred: "DELETE FROM #{@table_df} WHERE round=?1"
    }

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

  def receive(conn, recv_stmt, to_id, token, amount, timestamp)
      when is_reference(conn) do
    Sqlite3NIF.bind_and_step(conn, recv_stmt, [to_id, token, amount, timestamp])
  end
end
