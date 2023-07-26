defmodule AccountStore do
  @table "account"

  use Store.Sqlite,
    base: :account,
    table: @table,
    create: ~c"
    CREATE TABLE IF NOT EXISTS #{@table}(
    id BLOB PRIMARY KEY NOT NULL,
    validator BIGINT NOT NULL,
    pubkey BLOB NOT NULL,
    type_sig INTEGER NOT NULL,
    created_at BIGINT NOT NULL
    ) WITHOUT ROWID;
    ",
    stmt: %{
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5)",
      lookup: ~c"SELECT * FROM #{@table} WHERE id=?",
      validator: ~c"SELECT * FROM #{@table} WHERE id=? AND validator=?",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id=?",
      delete: ~c"DELETE FROM #{@table} WHERE id=?"
    }
end
