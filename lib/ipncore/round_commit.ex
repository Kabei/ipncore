defmodule RoundCommit do
  require Sqlite

  def sync(db_ref, tx_count, is_some_block_mine) do
    if tx_count > 0 do
      [
        Task.async(fn ->
          Sqlite.sync(db_ref)
        end),
        Task.async(fn ->
          wallet_dets = DetsPlux.get(:wallet)
          wallet_tx = DetsPlux.tx(:wallet)
          nonce_dets = DetsPlux.get(:nonce)
          nonce_tx = DetsPlux.tx(nonce_dets, :nonce)
          DetsPlux.sync(wallet_dets, wallet_tx)
          DetsPlux.sync(nonce_dets, nonce_tx)
        end),
        Task.async(fn ->
          balance_dets = DetsPlux.get(:balance)
          balance_tx = DetsPlux.tx(:balance)
          DetsPlux.sync(balance_dets, balance_tx)
        end),
        Task.async(fn ->
          stats_dets = DetsPlux.get(:stats)
          supply_tx = DetsPlux.tx(stats_dets, :supply)
          DetsPlux.sync(stats_dets, supply_tx)
        end)
      ]
      |> Task.await_many(:infinity)

      if is_some_block_mine do
        MemTables.clear_cache()
      end
    else
      balance_dets = DetsPlux.get(:balance)
      balance_tx = DetsPlux.tx(balance_dets, :balance)
      stats_dets = DetsPlux.get(:stats)
      supply_tx = DetsPlux.tx(stats_dets, :supply)
      DetsPlux.sync(balance_dets, balance_tx)
      DetsPlux.sync(stats_dets, supply_tx)
      Sqlite.sync(db_ref)
    end

    clear_cache()
  end

  def rollback(db_ref) do
    balance_tx = DetsPlux.tx(:balance)
    supply_tx = DetsPlux.tx(:stats, :supply)
    wallet_tx = DetsPlux.tx(:wallet)

    Sqlite.rollback(db_ref)
    Sqlite.begin(db_ref)
    DetsPlux.rollback(wallet_tx)
    DetsPlux.rollback(balance_tx)
    DetsPlux.rollback(supply_tx)
    clear_cache()
  end

  defp clear_cache do
    cache_wallet_tx = DetsPlux.tx(:wallet, :cache_wallet)
    cache_balance_tx = DetsPlux.tx(:balance, :cache_balance)
    cache_nonce_tx = DetsPlux.tx(:nonce, :cache_nonce)
    cache_supply = DetsPlux.tx(:stats, :cache_supply)
    DetsPlux.clear_tx(cache_wallet_tx)
    DetsPlux.clear_tx(cache_balance_tx)
    DetsPlux.clear_tx(cache_nonce_tx)
    DetsPlux.clear_tx(cache_supply)
  end
end
