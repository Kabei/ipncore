defmodule RefundStore do
  @table "refunds"

  use Store.Sqlite2,
    base: :refund,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      hash BLOB PRIMARY KEY NOT NULL,
      sender BLOB NOT NULL,
      `to` BLOB NOT NULL,
      token TEXT NOT NULL,
      amount BIGINT NOT NULL,
      expiry_in BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      "delete_expiry" => ~c"DELETE FROM #{@table} WHERE expiry_in < ?1",
      insert: ~c"REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6)",
      lookup:
        ~c"SELECT sender, token, amount FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3",
      exists: ~c"SELECT 1 FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3",
      delete: ~c"DELETE FROM #{@table} WHERE hash = ?"
    }

  # "DELETE FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3 RETURNING sender, token, amount",
end
