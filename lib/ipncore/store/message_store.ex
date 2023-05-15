defmodule MessageStore do
  @table "msg"

  use Store.Sqlite,
    base: :msg,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      hash BLOB PRIMARY KEY,
      message BLOB,
      signature BLOB,
      size INTEGER DEFAULT 0
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT OR IGNORE INTO #{@table} VALUES(?1,?2,?3,?4)",
      lookup: "SELECT * FROM #{@table} WHERE hash = ?1",
      exists: "SELECT 1 FROM #{@table} WHERE hash = ?1",
      delete: "DELETE FROM #{@table} WHERE hash = ?1"
    }
end
