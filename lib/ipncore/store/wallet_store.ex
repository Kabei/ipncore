defmodule WalletStore do
  @table "wallet"

  alias Ippan.Wallet

  use Store.Sqlite2,
    base: :wallet,
    table: @table,
    cache: true,
    mod: Ippan.Wallet,
    create: [~c"
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT PRIMARY KEY NOT NULL,
      pubkey BLOB NOT NULL,
      validator BIGINT NOT NULL,
      created_at BIGINT NOT NULL
    ) WITHOUT ROWID;"],
    stmt: %{
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4)",
      validator: ~c"SELECT pubkey, validator FROM #{@table} WHERE id=?1 AND validator=?2",
      lookup: ~c"SELECT pubkey, validator FROM #{@table} WHERE id=?",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id=?",
      delete: ~c"DELETE FROM #{@table} WHERE id=?",
      jackpot:
        ~c"SELECT pos, id FROM (SELECT ROW_NUMBER() OVER () AS pos, id FROM #{@table} ORDER BY created_at ASC) WHERE pos = ?",
      total: ~c"SELECT count(1) FROM #{@table}"
    }

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
