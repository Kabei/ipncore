defmodule MessageStore do
  @table "msg"
  @table_df "msgd"
  @table_hash "hashmap"
  @table_hashd "hashmapd"

  @expiry_time :timer.hours(24)
  @every div(@expiry_time, Application.compile_env(:ipncore, :block_interval))

  use Store.Sqlite2,
    base: :msg,
    table: @table,
    create: [
      "CREATE TABLE IF NOT EXISTS #{@table}(
        hash BLOB,
        timestamp BIGINT,
        type INTEGER,
        account_id BLOB,
        validator_id BIGINT,
        node_id BIGINT,
        args BLOB,
        message BLOB,
        signature BLOB,
        size INTEGER DEFAULT 0,
        PRIMARY KEY(timestamp, hash)
      )",
      "CREATE TABLE IF NOT EXISTS #{@table_df}(
        hash BLOB,
        timestamp BIGINT,
        key BLOB,
        type INTEGER,
        account_id BLOB,
        validator_id BIGINT,
        node_id BIGINT,
        args BLOB,
        message BLOB,
        signature BLOB,
        size INTEGER DEFAULT 0,
        round BIGINT,
        PRIMARY KEY(timestamp, hash),
        UNIQUE(key, type)
      )",
      "CREATE TABLE IF NOT EXISTS #{@table_hash}(
        hash BLOB NOT NULL,
        validator_id BIGINT NOT NULL,
        timestamp BIGINT,
        PRIMARY KEY(hash, validator_id)
      ) WITHOUT ROWID",
      "CREATE TABLE IF NOT EXISTS #{@table_hashd}(
        hash BLOB PRIMARY KEY NOT NULL,
        timestamp BIGINT
      ) WITHOUT ROWID",
      # triggers
      "CREATE TRIGGER IF NOT EXISTS tg_insert_hash_msg BEFORE INSERT ON #{@table}
      BEGIN
          INSERT INTO #{@table_hash} VALUES(NEW.hash, NEW.node_id, NEW.timestamp);
      END;",
      "CREATE TRIGGER IF NOT EXISTS tg_insert_hash_msgd BEFORE INSERT ON #{@table_df}
      BEGIN
          INSERT INTO #{@table_hashd} VALUES(NEW.hash, NEW.timestamp);
      END;"
    ],
    stmt: %{
      "all_df" => ~c"SELECT * FROM #{@table_df}",
      "all_hash" => ~c"SELECT * FROM #{@table_hash}",
      "select" =>
        ~c"SELECT hash, timestamp, type, account_id, validator_id, node_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, *  FROM #{@table})
        WHERE total <= ?1 AND node_id = ?2 ORDER BY hash",
      "select_df" =>
        ~c"SELECT hash, timestamp, key, type, account_id, validator_id, node_id, args, message, signature, size, ROWID
        FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, ROWID, * FROM #{@table_df} WHERE round IS NULL)
        WHERE total <= ?1 AND node_id = ?2 ORDER BY hash",
      "select_all_df_approved" =>
        ~c"SELECT hash, timestamp, key, type, account_id, validator_id, node_id, args, message, signature, size FROM #{@table_df} WHERE round = ?1 ORDER BY timestamp, hash",
      "delete_all" => ~c"DELETE FROM #{@table} WHERE ROWID <= ?1",
      "delete_all_df" => ~c"DELETE FROM #{@table_df} WHERE ROWID <= ?1 AND round IS NULL",
      "delete_all_df_approved" => ~c"DELETE FROM #{@table_df} WHERE round = ?1",
      "delete_hash" => ~c"DELETE FROM #{@table} WHERE hash = ?",
      "delete_hash_df" => ~c"DELETE FROM #{@table_df} WHERE hash = ?",
      "delete_df" => ~c"DELETE FROM #{@table_df} WHERE timestamp = ? AND hash = ?",
      "approve_df" => ~c"UPDATE #{@table_df} SET round=?1 WHERE timestamp = ?2 AND hash = ?3",
      "insert_df" =>
        ~c"INSERT INTO #{@table_df} (hash,timestamp,key,type,account_id,validator_id,node_id,args,message,signature,size) VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)
      ON CONFLICT (key,type) DO UPDATE SET timestamp=?3, hash=?4, account_id=?5, validator_id=?6, node_id=?7, args=?8, message=?9, signature=?10, size=?11
      WHERE timestamp > EXCLUDED.timestamp OR timestamp = EXCLUDED.timestamp AND hash > EXCLUDED.hash",
      # "insert_hash" => ~c"INSERT INTO #{@table_hash} VALUES(?,?,?)",
      "delete_expiry" => ~c"DELETE FROM #{@table_hash} WHERE timestamp < (? - #{@expiry_time})",
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)"
    }

  def all_df do
    call({:fetch, "all_df", []})
  end

  def all_hash do
    call({:fetch, "all_hash", []})
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
    call({:step, "delete_all_df_approved", [round]})
  end

  def select_all_df_approved(round) do
    call({:fetch, "select_all_df_approved", [round]})
  end

  def insert(params) do
    call({:step, :insert, params})
  end

  def insert_df(params) do
    call({:step, "insert_df", params})
  end

  # def insert_hash(hash, validator_id, timestamp) do
  #   call({:changes, "insert_hash", [hash, validator_id, timestamp]})
  # end

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

  def delete_expiry(round, timestamp) do
    if round != 0 and rem(round, @every) == 0 do
      call({:step, "delete_expiry", [timestamp]})
      sync()
    end
  end
end
