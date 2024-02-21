defmodule Stats do
  @db :stats
  @tx :stats

  def new do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)

    %{db: db, tx: tx}
  end

  def rounds(%{db: db, tx: tx}) do
    DetsPlux.get_cache(db, tx, "rounds", 0)
  end

  def rounds(%{db: db, tx: tx}, default) do
    DetsPlux.get_cache(db, tx, "rounds", default)
  end

  def put_round(%{tx: tx}, round_id) do
    DetsPlux.put(tx, "rounds", round_id)
  end

  def last_hash(%{db: db, tx: tx}) do
    DetsPlux.get_cache(db, tx, "last_hash", nil)
  end

  def put_last_hash(%{tx: tx}, hash) do
    DetsPlux.put(tx, "last_hash", hash)
  end

  def txs(%{db: db, tx: tx}) do
    DetsPlux.get_cache(db, tx, "txs", 0)
  end

  def count_txs(_, 0), do: :ok

  def count_txs(%{tx: tx}, number) do
    DetsPlux.update_counter(tx, "txs", {2, number}, {2, 0})
  end

  def blocks(%{db: db, tx: tx}) do
    DetsPlux.get_cache(db, tx, "blocks", 0)
  end

  def count_blocks(_, 0), do: :ok

  def count_blocks(%{tx: tx}, number) do
    DetsPlux.update_counter(tx, "blocks", {2, number}, {2, 0})
  end
end
