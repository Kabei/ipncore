defmodule DomainStore do
  @table "domain"

  use Store.Sqlite,
    base: :domain,
    table: @table,
    mod: Ippan.Domain,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
    name TEXT PRIMARY KEY NOT NULL,
    owner BLOB NOT NULL,
    email TEXT,
    avatar TEXT,
    records UNSIGNED INTEGER DEFAULT 0,
    enabled BOOLEAN DEFAULT TRUE,
    created_at UNSIGNED BIGINT NOT NULL,
    renewed_at UNSIGNED BIGINT NOT NULL,
    updated_at UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
      replace: "REPLACE INTO #{@table} VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
      lookup: "SELECT * FROM #{@table} WHERE name = ?",
      exists: "SELECT 1 FROM #{@table} WHERE name = ?",
      delete: "DELETE FROM #{@table} WHERE name = ?"
    }
end
