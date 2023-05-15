defmodule GlobalRequestStore do
  @table "request"

  use Store.Sqlite,
    base: :grs,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      type INTEGER NOT NULL,
      id BLOB,
      hash BLOB NOT NULL,
      timestamp UNSIGNED BIGINT NOT NULL,
      size BIGINT DEFAULT 0,
      PRIMARY KEY(type, id)
    ) WITHOUT ROWID;
    """,
    stmt: %{
      # "delete_round" => "DELETE FROM #{@table} WHERE round = ?",
      insert: """
      INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5) ON CONFLICT (type, id)
      DO UPDATE SET hash = ?3, timestamp = ?4, size=?5
      WHERE timestamp > ?4 OR timestamp == ?4 AND hash > ?3
      """,
      lookup: "SELECT * FROM #{@table} WHERE type = ?1 AND id = ?2",
      exists: "SELECT 1 FROM #{@table} WHERE type = ?1 AND id = ?2",
      delete: "DELETE FROM #{@table} WHERE type = ?1 AND id = ?2"
    }
end
