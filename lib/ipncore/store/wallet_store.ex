defmodule WalletStore do
  @table "wallet"

  use Store.Sqlite,
    base: :wallet,
    table: @table,
    create: "
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT PRIMARY KEY NOT NULL,
      pubkey BLOB NOT NULL,
      validator UNSIGNED INTEGER NOT NULL,
      created_at UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    ",
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4)",
      validator: "SELECT pubkey, validator FROM #{@table} WHERE id=?1 AND validator=?2",
      lookup: "SELECT pubkey, validator FROM #{@table} WHERE id=?",
      exists: "SELECT 1 FROM #{@table} WHERE id=?",
      delete: "DELETE FROM #{@table} WHERE id=?"
    }
end
