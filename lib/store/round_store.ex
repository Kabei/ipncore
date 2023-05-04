defmodule RoundStore do
  @table "round"

  use Store.Sqlite,
    base: :block,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id UNSIGNED BIGINT PRIMARY KEY NOT NULL,
      blocks UNSIGNED INTEGER NOT NULL,
      timestamp UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      delete: "DELETE FROM #{@table} WHERE id = ?"
    }
end
