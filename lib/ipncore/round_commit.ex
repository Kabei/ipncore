defmodule RoundCommit do
  # @moduledoc """
  # Save all once at a time
  # """
  # require SqliteStore
  # use GenServer

  # def start_link(args) do
  #   case Process.whereis(__MODULE__) do
  #     nil ->
  #       GenServer.start_link(__MODULE__, args, name: __MODULE__)

  #     pid ->
  #       {:ok, pid}
  #   end
  # end

  # @impl true
  # def init(_args) do
  #   {:ok, %{sync: false, repeat: false}}
  # end

  # @impl true
  # def handle_cast(:sync, state = %{sync: true}) do
  #   {:noreply, %{state | repeat: true}}
  # end

  # def handle_cast(
  #       :sync,
  #       state = %{conn: conn, balance: balance_dets, stats: stats_dets, wallet: wallet_dets}
  #     ) do
  #   spawn_commit(conn, wallet_dets, balance_dets, stats_dets)

  #   {:noreply, %{state | sync: true}}
  # end

  # def handle_cast(
  #       :rollback,
  #       state = %{conn: conn}
  #     ) do
  #   rollback(conn)

  #   {:noreply, state}
  # end

  # def handle_cast(
  #       :done,
  #       state = %{
  #         conn: conn,
  #         balance: balance_dets,
  #         repeat: repeat,
  #         stats: stats_dets,
  #         wallet: wallet_dets
  #       }
  #     ) do
  #   if repeat do
  #     spawn_commit(conn, wallet_dets, balance_dets, stats_dets)
  #   end

  #   {:noreply, %{state | sync: false, repeat: false}}
  # end
  require SqliteStore

  def sync(conn, tx_count) do
    if tx_count > 0 do
      [
        Task.async(fn -> SqliteStore.commit(conn) end),
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

      clear_cache()
    else
      SqliteStore.commit(conn)
    end

    # GenServer.cast(__MODULE__, :sync)
  end

  # def stop do
  #   GenServer.stop(__MODULE__)
  # end

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
    cache_nonce_tx = DetsPlux.tx(:cache_nonce)
    DetsPlux.clear_tx(cache_nonce_tx)
    MemTables.clear_cache()
  end
end
