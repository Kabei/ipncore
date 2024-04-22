defmodule Ippan.DetsSup do
  use Supervisor

  def start_link(children) do
    Supervisor.start_link(__MODULE__, children, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    store_dir = :persistent_term.get(:store_dir)
    wallet_path = Path.join(store_dir, "wallet.dx")
    nonce_path = Path.join(store_dir, "nonce.dx")
    balance_path = Path.join(store_dir, "balance.dx")
    stats_path = Path.join(store_dir, "stats.dx")

    children = [
      {DetsPlux, [id: :wallet, file: wallet_path]},
      {DetsPlux, [id: :nonce, file: nonce_path]},
      {DetsPlux, [id: :balance, file: balance_path]},
      {DetsPlux, [id: :stats, file: stats_path]}
    ]

    Supervisor.init(children, strategy: :one_for_one)
  end

  def dets do
    %{
      balance: DetsPlux.get(:balance),
      nonce: DetsPlux.get(:nonce),
      stats: DetsPlux.get(:stats),
      wallet: DetsPlux.get(:wallet)
    }
  end

  def txs do
    %{
      balance: DetsPlux.tx(:balance),
      nonce: DetsPlux.tx(:nonce),
      stats: DetsPlux.tx(:stats),
      wallet: DetsPlux.tx(:wallet)
    }
  end
end
