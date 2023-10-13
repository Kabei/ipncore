defmodule TokenSupply do
  @db :stats

  @spec get(tx :: DetsPlux.transaction(), token_id :: binary) :: supply :: integer()
  def get(tx, token_id) do
    key = DetsPlux.tuple(token_id, "supply")
    dets = DetsPlux.get(@db)
    DetsPlux.get_tx(dets, tx, key, 0)
  end

  @spec fetch(tx :: DetsPlux.transaction(), token_id :: binary) ::
          {DetsPlux.key(), supply :: integer}
  def fetch(tx, token_id) do
    key = DetsPlux.tuple(token_id, "supply")
    dets = DetsPlux.get(@db)
    {key, DetsPlux.get_tx(dets, tx, key, 0)}
  end

  @spec set(DetsPlux.transaction(), DetsPlux.key(), integer()) :: true
  def set(tx, key, amount) do
    DetsPlux.put(tx, key, amount)
  end

  @spec add(DetsPlux.transaction(), DetsPlux.key(), integer(), integer()) :: true
  def add(tx, key, supply, amount) do
    DetsPlux.put(tx, key, supply + amount)
  end

  @spec add_if(DetsPlux.transaction(), DetsPlux.key(), integer(), integer(), pos_integer()) ::
          true
  def add_if(tx, key, supply, amount, 0) do
    DetsPlux.put(tx, key, supply + amount)
  end

  def add_if(tx, key, supply, amount, max_supply) do
    total = supply + amount

    if max_supply >= total do
      DetsPlux.put(tx, key, supply + amount)
    else
      false
    end
  end

  def valid?(supply, amount, max_supply) do
    max_supply >= supply + amount
  end

  @spec subtract(DetsPlux.transaction(), DetsPlux.key(), integer(), integer()) :: true
  def subtract(tx, key, supply, amount) do
    DetsPlux.put(tx, key, supply - amount)
  end
end
