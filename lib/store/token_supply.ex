defmodule TokenSupply do
  @table :stats

  @spec get(tx :: DetsPlux.transaction(), token_id :: binary) :: supply :: integer()
  def get(tx, token_id) do
    key = DetsPlux.tuple(token_id, "supply")
    DetsPlux.get_tx(@table, tx, key, 0)
  end

  @spec fetch(tx :: DetsPlux.transaction(), token_id :: binary) ::
          {DetsPlux.key(), supply :: integer}
  def fetch(tx, token_id) do
    key = DetsPlux.tuple(token_id, "supply")
    {key, DetsPlux.get_tx(@table, tx, key, 0)}
  end

  @spec add(DetsPlux.transaction(), DetsPlux.key(), integer(), integer()) :: true
  def add(tx, key, supply, amount) do
    DetsPlux.put(tx, key, supply + amount)
  end

  @spec subtract(DetsPlux.transaction(), DetsPlux.key(), integer(), integer()) :: true
  def subtract(tx, key, supply, amount) do
    DetsPlux.put(tx, key, supply - amount)
  end
end
