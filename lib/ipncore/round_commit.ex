defmodule RoundCommit do
  require SqliteStore

  def sync(conn, tx_count, is_some_block_mine) do
    IO.puts("RoundCommit: Sync #{tx_count} | #{is_some_block_mine}")

    if tx_count > 0 do
      r =
        [
          Task.async(fn ->
            SqliteStore.commit(conn)
            SqliteStore.begin(conn)
          end),
          Task.async(fn ->
            wallet_dets = DetsPlux.get(:wallet)
            wallet_tx = DetsPlux.tx(:wallet)
            nonce_tx = DetsPlux.tx(:nonce)
            DetsPlux.sync(wallet_dets, wallet_tx)
            DetsPlux.sync(wallet_dets, nonce_tx)
          end),
          Task.async(fn ->
            balance_dets = DetsPlux.get(:balance)
            balance_tx = DetsPlux.tx(:balance)
            DetsPlux.sync(balance_dets, balance_tx)
          end),
          Task.async(fn ->
            stats_dets = DetsPlux.get(:stats)
            supply_tx = DetsPlux.tx(:supply)
            DetsPlux.sync(stats_dets, supply_tx)
          end)
        ]
        |> Task.await_many(:infinity)

      IO.inspect(r)

      if is_some_block_mine do
        clear_cache()
      end
    else
      SqliteStore.commit(conn)
      SqliteStore.begin(conn)
    end
  end

  def rollback(conn) do
    balance_tx = DetsPlux.tx(:balance)
    supply_tx = DetsPlux.tx(:supply)
    wallet_tx = DetsPlux.tx(:wallet)

    SqliteStore.rollback(conn)
    DetsPlux.rollback(wallet_tx)
    DetsPlux.rollback(balance_tx)
    DetsPlux.rollback(supply_tx)
    clear_cache()
  end

  defp clear_cache do
    cache_wallet_tx = DetsPlux.tx(:cache_wallet)
    cache_nonce_tx = DetsPlux.tx(:cache_nonce)
    DetsPlux.clear_tx(cache_wallet_tx)
    DetsPlux.clear_tx(cache_nonce_tx)
    MemTables.clear_cache()
  end
end
