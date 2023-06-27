defmodule MessageStore do
  @table "msg"
  @table_df "msg_df"
  @table_pre "msg_pre"

  use Store.Sqlite,
    base: :msg,
    # pool: :msg_pool,
    table: @table,
    create: [
      """
      CREATE TABLE IF NOT EXISTS #{@table}(
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
      );
      """,
      """
      CREATE TABLE IF NOT EXISTS #{@table_df}(
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
        PRIMARY KEY(key, type)
      );
      """
    ],
    stmt: %{
      # by size
      "select" =>
        "SELECT timestamp, hash, type, account_id, validator_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, *, ROWID FROM #{@table})
        WHERE total <= ?1",
      "select_df" =>
        "SELECT key, type, timestamp, hash, account_id, validator_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, *, ROWID FROM #{@table_df} WHERE round IS NULL)
        WHERE total <= ?1",
      "delete_all" => "DELETE FROM #{@table} WHERE ROWID <= ?1",
      "delete_all_df" => "DELETE FROM #{@table_df} WHERE round <= ?1",
      "delete" => "DELETE FROM #{@table} WHERE hash = ?",
      "delete_df" => "DELETE FROM #{@table_df} WHERE hash = ?",
      "approve_df" => "UPDATE #{@table_df} SET round=?1 WHERE timestamp = ?2 AND hash = ?3",
      "insert_df" =>
        "INSERT INTO #{@table_df} (key,type,timestamp,hash,account_id,validator_id,args,message,signature,size) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)
      ON CONFLICT (key,type) DO UPDATE SET timestamp=?3, hash=?4, account_id=?5, validator_id=?6, args=?7, message=?8, signature=?9, size=?10
      WHERE timestamp > EXCLUDED.timestamp OR timestamp = EXCLUDED.timestamp AND hash > EXCLUDED.hash",
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)"
      # "nullable" => "REPLACE INTO #{@table}(hash,type) VALUES(?1,-1)",
      # "recover" => "SELECT * FROM #{@table} WHERE type != -1",
      # "delete_null" => "DELETE FROM #{@table} WHERE type = -1",
      # lookup: "SELECT * FROM #{@table} WHERE hash = ?1",
      # exists: "SELECT 1 FROM #{@table} WHERE hash = ?1",
      # delete: "DELETE FROM #{@table} WHERE hash = ?1"
    }

  def select(size) do
    call({:execute_fetch, "select", [size]})
  end

  def select_df(size) do
    call({:execute_fetch, "select_df", [size]})
  end

  def delete_all(rowid) do
    call({:execute_step, "delete_all", [rowid]})
  end

  def delete_all_df(rowid) do
    call({:execute_step, "delete_all_df", [rowid]})
  end

  # def set_nullable(hash) do
  #   call({:execute_step, "nullable", [hash]})
  # end

  # def recover_all do
  #   call({:execute_fetch, "recover", []})
  # end

  # def delete_null do
  #   call({:execute_step, "delete_null", []})
  # end

  def insert_df(params) do
    call({:execute_changes, "insert_df", params})
  end

  def delete_only(hash) do
    call({:execute_step, "delete", [hash]})
  end

  def delete_df(hash) do
    call({:execute_step, "delete_def", [hash]})
  end

  def approve_def(round, timestamp, hash) do
    call({:execute_step, "approve_def", [round, timestamp, hash]})
  end
end
