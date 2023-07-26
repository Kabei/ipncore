defmodule MessageStore do
  @table "msg"
  @table_df "msg_df"

  use Store.Sqlite2,
    base: :msg,
    table: @table,
    create: [
      "CREATE TABLE IF NOT EXISTS #{@table}(
        timestamp BIGINT,
        hash BLOB,
        type INTEGER,
        account_id BLOB,
        validator_id BIGINT,
        args BLOB,
        message BLOB,
        signature BLOB,
        size INTEGER DEFAULT 0,
        PRIMARY KEY(timestamp, hash)
      )",
      "CREATE TABLE IF NOT EXISTS #{@table_df}(
        key BLOB,
        type INTEGER,
        timestamp BIGINT,
        hash BLOB,
        account_id BLOB,
        validator_id BIGINT,
        args BLOB,
        message BLOB,
        signature BLOB,
        size INTEGER DEFAULT 0,
        round BIGINT,
        PRIMARY KEY(timestamp, hash),
        UNIQUE(key, type)
      )"
    ],
    stmt: %{
      # by size
      "all_df" => ~c"SELECT * FROM #{@table_df}",
      "select" =>
        ~c"SELECT timestamp, hash, type, account_id, validator_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, *  FROM #{@table})
        WHERE total <= ?1 and validator_id = ?2 ORDER BY timestamp, hash",
      "select_df" =>
        ~c"SELECT key, type, timestamp, hash, account_id, validator_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, * FROM #{@table_df} WHERE round IS NULL)
        WHERE total <= ?1 and validator_id = ?2 ORDER BY timestamp, hash",
      "delete_all" => ~c"DELETE FROM #{@table} WHERE ROWID <= ?1",
      "delete_all_df" => ~c"DELETE FROM #{@table_df} WHERE ROWID <= ?1 AND round IS NULL",
      "delete_all_df_approved" =>
        ~c"DELETE FROM #{@table_df} WHERE round = ?1 RETURNING key, type, timestamp, hash, account_id, validator_id, args, message, signature, size",
      "delete_hash" => ~c"DELETE FROM #{@table} WHERE hash = ?",
      "delete_hash_df" => ~c"DELETE FROM #{@table_df} WHERE hash = ?",
      "delete_df" => ~c"DELETE FROM #{@table_df} WHERE timestamp = ? AND hash = ?",
      "approve_df" => ~c"UPDATE #{@table_df} SET round=?1 WHERE timestamp = ?2 AND hash = ?3",
      "insert_df" =>
        ~c"INSERT INTO #{@table_df} (key,type,timestamp,hash,account_id,validator_id,args,message,signature,size) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)
      ON CONFLICT (key,type) DO UPDATE SET timestamp=?3, hash=?4, account_id=?5, validator_id=?6, args=?7, message=?8, signature=?9, size=?10
      WHERE timestamp > EXCLUDED.timestamp OR timestamp = EXCLUDED.timestamp AND hash > EXCLUDED.hash",
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
    }

  def all_df do
    call({:fetch, "all_df", []})
  end

  def select(size, validator_id) do
    call({:fetch, "select", [size, validator_id]})
  end

  def select_df(size, validator_id) do
    call({:fetch, "select_df", [size, validator_id]})
  end

  def delete_all(nil), do: :ok
  def delete_all(-1), do: :ok

  def delete_all(rowid) do
    call({:step, "delete_all", [rowid]})
  end

  def delete_all_df(nil), do: :ok
  def delete_all_df(-1), do: :ok

  def delete_all_df(rowid) do
    call({:step, "delete_all_df", [rowid]})
  end

  def delete_all_df_approved(round) do
    call({:fetch, "delete_all_df_approved", [round]})
  end

  def insert_df(params) do
    call({:changes, "insert_df", params})
  end

  def delete(hash) do
    call({:step, "delete_hash", [hash]})
  end

  def delete_df(hash) do
    call({:step, "delete_hash_df", [hash]})
  end

  def delete_df(timestamp, hash) do
    call({:step, "delete_df", [timestamp, hash]})
  end

  def approve_df(round, timestamp, hash) do
    call({:step, "approve_df", [round, timestamp, hash]})
  end

  def delete_not_approve_df(round) do
    call({:step, "delete_not_approve_df", [round]})
  end
end
