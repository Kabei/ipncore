defmodule AccountStore do
  @table "account"

  use Store.Sqlite,
    base: :account,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
    id TEXT PRIMARY KEY NOT NULL,
    validator UNSIGNED INTEGER NOT NULL,
    address TEXT,
    auth_hash BLOB NOT NULL,
    pubkey BLOB NOT NULL,
    created_at UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT OR REPLACE INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      delete: "DELETE FROM #{@table} WHERE id = ?"
    }
end
