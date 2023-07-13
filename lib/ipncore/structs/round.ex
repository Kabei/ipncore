defmodule Ippan.Round do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          blocks: non_neg_integer(),
          timestamp: non_neg_integer()
        }

  defstruct [:id, :hash, :prev, :blocks, :timestamp]

  def to_list(x) do
    [x.id, x.hash, x.prev, x.blocks, x.timestamp]
  end

  def to_tuple(x) do
    {x.id, x.hash, x.prev, x.blocks, x.timestamp}
  end

  def to_map({id, hash, prev, blocks, timestamp}) do
    %{id: id, hash: hash, prev: prev, blocks: blocks, timestamp: timestamp}
  end

  def to_map([id, hash, prev, blocks, timestamp]) do
    %{id: id, hash: hash, prev: prev, blocks: blocks, timestamp: timestamp}
  end

  def compute_hash(round, prev, hashes) do
    ([
       to_string(round),
       normalize(prev)
     ] ++
       hashes)
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
