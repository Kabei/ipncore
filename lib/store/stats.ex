defmodule Stats do
  @db :stats
  @tx :stats
  @cache :cache_stats

  def new do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)

    %{db: db, tx: tx}
  end

  def new(tx) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, tx)

    %{db: db, tx: tx}
  end

  def cache do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @cache)

    %{db: db, tx: tx}
  end

  def get(%{db: db, tx: tx}, key) do
    DetsPlux.get_cache(db, tx, key, 0)
  end

  def get(%{db: db, tx: tx}, key, default) do
    DetsPlux.get_cache(db, tx, key) || default
  end

  def put(%{tx: tx}, key, value) do
    DetsPlux.put(tx, key, value)
  end

  def incr(%{db: db, tx: tx}, key, number) do
    DetsPlux.get_cache(db, tx, key, 0)
    DetsPlux.update_counter(tx, key, {2, number})
  end

  def sync(%{db: db, tx: tx}) do
    DetsPlux.sync(db, tx)
  end
end
