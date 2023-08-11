defmodule DnsStore do
  @table "dns"
  @args %{"table" => @table}

  use Store.Sqlite2,
    base: :dns,
    table: @table,
    mod: Ippan.DNS,
    create: SQL.readFile!("lib/sql/dns.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/dns.stmt.sql", @args)
end
