defmodule RoundStore do
  @table "round"

  use Store.Sqlite,
    base: :round,
    table: @table,
    mod: Ippan.Round,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hash BLOB,
      blocks BIGINT NOT NULL,
      timestamp BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      delete: "DELETE FROM #{@table} WHERE id = ?"
    }
end
