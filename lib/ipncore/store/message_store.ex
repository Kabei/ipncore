defmodule MessageStore do
  @table "msg"

  use Store.Sqlite,
    base: :msg,
    # pool: :msg_pool,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      hash BLOB PRIMARY KEY,
      type INTEGER,
      timestamp BIGINT,
      account_id BLOB,
      validator_id BLOB,
      args BLOB,
      message BLOB,
      signature BLOB,
      size INTEGER DEFAULT 0
    );
    """,
    stmt: %{
      "by_size" =>
        "SELECT hash, type, timestamp, account_id, validator_id, args, message, signature, size FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, * FROM msg) WHERE total <= ?1",
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9)",
      lookup: "SELECT * FROM #{@table} WHERE hash = ?1",
      exists: "SELECT 1 FROM #{@table} WHERE hash = ?1",
      delete: "DELETE FROM #{@table} WHERE hash = ?1"
    }

  def fetch_by_size(size) do
    call({:execute_fetch, "by_size", [size]})
  end
end
