defmodule TokenSupply do
  @db :stats
  @tx :supply
  @cache_tx :cache_supply
  @word "supply"

  defstruct db: nil, tx: nil, key: nil

  defmacrop key(token_id) do
    quote do
      DetsPlux.tuple(unquote(token_id), @word)
    end
  end

  def new(id) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)
    key = key(id)

    %__MODULE__{
      db: db,
      tx: tx,
      key: key
    }
  end

  def cache(id) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @cache_tx)
    key = key(id)

    %__MODULE__{
      db: db,
      tx: tx,
      key: key
    }
  end

  def get(%{db: db, tx: tx, key: key}) do
    DetsPlux.get_tx(db, tx, key, 0)
  end

  @spec get(DetsPlux.db(), DetsPlux.transaction(), binary) :: supply :: integer()
  def get(db, tx, key) do
    DetsPlux.get_tx(db, tx, key, 0)
  end

  @spec put(map, integer()) :: true
  def put(%{tx: tx, key: key}, amount) do
    DetsPlux.put(tx, key, amount)
  end

  @spec add(map, integer()) :: true
  def add(%{db: db, tx: tx, key: key}, amount) do
    DetsPlux.put(tx, key, get(db, tx, key) + amount)
  end

  @spec subtract(map, integer()) :: true
  def subtract(%{db: db, tx: tx, key: key}, amount) do
    DetsPlux.put(tx, key, get(db, tx, key) - amount)
  end
end
