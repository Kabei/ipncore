defmodule TokenSupply do
  @table :stats

  defp gen_key(token_id) do
    "#{token_id}.supply"
  end

  @spec get(token_id :: binary) :: integer()
  def get(token_id) do
    key = gen_key(token_id)
    {_, supply} = DetsPlus.lookup(@table, key, 0)
    supply
  end

  @spec add(token_id :: binary, amount :: integer()) :: :ok | {:error, term()}
  def add(token_id, amount) do
    key = gen_key(token_id)
    {_, supply} = DetsPlus.lookup(@table, key, 0)
    DetsPlus.insert(@table, {key, supply + amount})
  end

  @spec subtract(token_id :: binary, amount :: integer()) :: :ok | {:error, term()}
  def subtract(token_id, amount) do
    key = gen_key(token_id)
    {_, supply} = DetsPlus.lookup(@table, key, 0)
    DetsPlus.insert(@table, {key, supply - amount})
  end
end
