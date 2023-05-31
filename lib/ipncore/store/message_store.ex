defmodule MessageStore do
  @table "msg"
  @block_max_size 10_000_000

  use Store.Sqlite,
    base: :msg,
    # pool: :msg_pool,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      hash BLOB PRIMARY KEY,
      message BLOB,
      signature BLOB,
      size INTEGER DEFAULT 0
    );
    """,
    stmt: %{
      "by_block" =>
        "SELECT message, signature FROM (SELECT sum(size) OVER (ORDER BY ROWID) as total, message, signature FROM msg) WHERE total <= #{@block_max_size}",
      insert: "INSERT OR IGNORE INTO #{@table} VALUES(?1,?2,?3,?4)",
      lookup: "SELECT * FROM #{@table} WHERE hash = ?1",
      exists: "SELECT 1 FROM #{@table} WHERE hash = ?1",
      delete: "DELETE FROM #{@table} WHERE hash = ?1"
    }
end
