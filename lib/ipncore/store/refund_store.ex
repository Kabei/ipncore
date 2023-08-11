defmodule RefundStore do
  @table "refunds"
  @args %{"table" => @table}

  use Store.Sqlite2,
    base: :refund,
    table: @table,
    create: SQL.readFile!("lib/sql/refund.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/refund.stmt.sql", @args)

  # "DELETE FROM #{@table} WHERE hash = ?1 AND `to` = ?2 AND expiry_in > ?3 RETURNING sender, token, amount",
end
