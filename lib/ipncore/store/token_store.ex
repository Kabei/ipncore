defmodule TokenStore do
  @table "token"

  @args %{
    "table" => @table
  }

  alias Ippan.Token

  use Store.Sqlite2,
    base: :token,
    mod: Ippan.Token,
    table: @table,
    create: SQL.readFile!("lib/sql/token.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/token.stmt.sql", @args)

  use Store.Cache,
    table: :token,
    mod: Token,
    mode: "partial",
    size: 10_000_000

  def sum_suppy(token, total) do
    call({:step, "sum_supply", [token, total]})
  end

  def sum_burned(token, total) do
    call({:step, "sum_burned", [token, total]})
  end

  def total do
    {_, [total]} = call({:step, :total, []})
    total
  end
end
