defmodule TokenSupply do
  @db :stats
  @tx :supply
  @cache_tx :cache_supply
  @word "supply"

  defstruct db: nil, id: nil, key: nil, tx: nil, output: []

  defmacrop key(token_id) do
    quote do
      DetsPlux.tuple(unquote(token_id), @word)
    end
  end

  def new(id) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)
    key = key(id)
    DetsPlux.get_cache(db, tx, key, 0)

    %__MODULE__{
      id: id,
      db: db,
      tx: tx,
      key: key
    }
  end

  def new(id, tx_name) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, tx_name)
    key = key(id)
    DetsPlux.get_cache(db, tx, key, 0)

    %__MODULE__{
      id: id,
      db: db,
      tx: tx,
      key: key
    }
  end

  def jackpot do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @tx)
    key = "jackpot|supply"
    DetsPlux.get_cache(db, tx, key, 0)

    %__MODULE__{
      id: "jackpot",
      db: db,
      tx: tx,
      key: key
    }
  end

  def cache(id) do
    db = DetsPlux.get(@db)
    tx = DetsPlux.tx(db, @cache_tx)
    key = key(id)
    DetsPlux.get_cache(db, tx, key, 0)

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
    DetsPlux.get_cache(db, tx, key, 0)
  end

  @spec put(map, integer()) :: true
  def put(%{tx: tx, key: key}, amount) do
    DetsPlux.put(tx, key, amount)
  end

  @spec add(map, number()) :: number()
  def add(%{tx: tx, key: key}, amount) do
    DetsPlux.update_counter(tx, key, amount)
  end

  @spec subtract(map, number()) :: number()
  def subtract(%{tx: tx, key: key}, amount) do
    DetsPlux.update_counter(tx, key, -amount)
  end

  @spec requires!(map, number(), number()) :: map | no_return()
  def requires!(ts, _amount, 0), do: ts

  def requires!(ts = %{tx: tx, key: key}, amount, max_supply) do
    if DetsPlux.update_counter(tx, key, amount) > max_supply do
      DetsPlux.update_counter(tx, key, -amount)
      raise IppanError, "max supply exceeded"
    else
      put_out(ts, amount, max_supply)
    end
  end

  def multi_requires!(db, tx, outputs) do
    Enum.reduce(outputs, [], fn
      {_key, _amount, 0}, acc ->
        acc

      {key, amount, max_supply}, acc ->
        DetsPlux.get_cache(db, tx, key, 0)

        if DetsPlux.update_counter(tx, key, amount) > max_supply do
          DetsPlux.update_counter(tx, key, -amount)

          Enum.each(acc, fn {key, value} ->
            DetsPlux.update_counter(tx, key, {2, value})
          end)

          raise IppanError, "Max supply exceeded"
        else
          [{key, amount} | acc]
        end
    end)
  end

  @spec delete(map) :: true
  def delete(%{tx: tx, key: key}) do
    DetsPlux.delete(tx, key)
  end

  def output(%TokenSupply{output: output}) do
    output
  end

  defp put_out(ts = %{key: key, output: output}, value, max_supply) do
    %{ts | output: [{key, value, max_supply} | output]}
  end
end
