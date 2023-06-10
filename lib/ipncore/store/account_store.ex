defmodule AccountStore do
  @table "account"

  use Store.Sqlite,
    base: :account,
    table: @table,
    create: "
    CREATE TABLE IF NOT EXISTS #{@table}(
    id BLOB PRIMARY KEY NOT NULL,
    validator UNSIGNED INTEGER NOT NULL,
    pubkey BLOB NOT NULL,
    type_sig INTEGER NOT NULL,
    created_at BIGINT NOT NULL
    ) WITHOUT ROWID;
    ",
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5) ON CONFLICT (id)
        DO UPDATE SET validator=?2, created_at=?3
        WHERE created_at > ?3",
      lookup: "SELECT * FROM #{@table} WHERE id=?",
      validator: "SELECT * FROM #{@table} WHERE id=? AND validator=?",
      exists: "SELECT 1 FROM #{@table} WHERE id=?",
      delete: "DELETE FROM #{@table} WHERE id=?"
    }
end
