defmodule Stats do
  require DetsPlux
  @db :stats
  @tx :stats

  def new do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)

    tx
  end

  def put_round(tx, round_id) do
    DetsPlux.put(tx, "rounds", round_id)
  end

  def count_txs(_tx, 0), do: :ok

  def count_txs(tx, number) do
    DetsPlux.update_counter(tx, "txs", {2, number})
  end

  def count_blocks(_tx, 0), do: :ok

  def count_blocks(tx, number) do
    DetsPlux.update_counter(tx, "blocks", {2, number})
  end
end
