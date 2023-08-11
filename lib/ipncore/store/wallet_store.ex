defmodule WalletStore do
  alias Ippan.Wallet

  @table "wallet"
  @args %{"table" => @table}

  use Store.Sqlite2,
    base: :wallet,
    table: @table,
    cache: true,
    mod: Ippan.Wallet,
    create: SQL.readFile!("lib/sql/wallet.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/wallet.stmt.sql", @args)

  use Store.Cache,
    table: :wallet,
    mod: Wallet,
    mode: "partial",
    size: 50_000_000

  def total do
    {_, [total]} = call({:step, :total, []})
    total
  end

  def jackpot(pos) do
    res = call({:step, :jackpot, [pos]})

    case res do
      {:row, [_pos, id]} -> id
      _ -> nil
    end
  end
end
