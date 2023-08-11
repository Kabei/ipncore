defmodule ValidatorStore do
  @table "validator"

  @args %{
    "table" => @table
  }

  use Store.Sqlite2,
    base: :validator,
    cache: true,
    mod: Ippan.Validator,
    table: @table,
    create: SQL.readFile!("lib/sql/validator.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/validator.stmt.sql", @args)

  use Store.Cache,
    table: :validator,
    mod: Ippan.Validator,
    mode: "partial",
    size: 10_000_000

  def total do
    {_, [total]} = call({:step, "total", []})
    total
  end
end
