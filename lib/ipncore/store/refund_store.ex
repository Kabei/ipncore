defmodule RefundStore do
  @table "refunds"

  use Store.Sqlite,
    base: :refund,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      hash BLOB PRIMARY KEY NOT NULL,
      sender BLOB NOT NULL,
      `to` BLOB NOT NULL,
      token TEXT NOT NULL,
      amount UNSIGNED BIGINT NOT NULL,
      expiry_in UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      "delete_expiry" => "DELETE FROM #{@table} WHERE expiry_in < ?1",
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6)",
      lookup:
        "SELECT sender, token, amount FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3",
      exists: "SELECT 1 FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3",
      delete: "DELETE FROM #{@table} WHERE hash = ?"
    }
end
