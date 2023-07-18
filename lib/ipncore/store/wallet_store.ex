defmodule WalletStore do
  @table "wallet"

  use Store.Sqlite,
    base: :wallet,
    # pool: :wallet_pool,
    table: @table,
    cache: true,
    cache_size: 50_000_000,
    mod: Ippan.Wallet,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id TEXT PRIMARY KEY NOT NULL,
      pubkey BLOB NOT NULL,
      validator BIGINT NOT NULL,
      created_at BIGINT NOT NULL
    ) WITHOUT ROWID;"],
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4)",
      validator: "SELECT pubkey, validator FROM #{@table} WHERE id=?1 AND validator=?2",
      lookup: "SELECT id, pubkey, validator FROM #{@table} WHERE id=?",
      exists: "SELECT 1 FROM #{@table} WHERE id=?",
      delete: "DELETE FROM #{@table} WHERE id=?",
      jackpot:
        "SELECT (SELECT row_number() AS pos, id FROM #{@table} ORDER BY created_at) WHERE pos = ?",
      total: "SELECT count(1) FROM #{@table}"
    }

  def total do
    {_, [total]} = call({:execute_step, :total, []})
    total
  end

  def jackpot(pos) do
    {:row, res} = call({:execute_step, :jackpot, [pos]})

    case res do
      [[_pos, id]] -> id
      _ -> nil
    end
  end
end
