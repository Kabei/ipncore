defmodule Ippan.Round do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          creator: non_neg_integer(),
          coinbase: non_neg_integer(),
          blocks: non_neg_integer(),
          timestamp: non_neg_integer()
        }

  defstruct [:id, :hash, :prev, :creator, :coinbase, :blocks, :timestamp]

  @impl true
  def to_list(x) do
    [x.id, x.hash, x.prev, x.creator, x.coinbase, x.blocks, x.timestamp]
  end

  @impl true
  def to_tuple(x) do
    {x.id, x}
  end

  @impl true
  def list_to_tuple([id | _] = x) do
    {id, list_to_map(x)}
  end

  @impl true
  def list_to_map([id, hash, prev, creator, coinbase, blocks, timestamp]) do
    %{
      id: id,
      hash: hash,
      prev: prev,
      creator: creator,
      coinbase: coinbase,
      blocks: blocks,
      timestamp: timestamp
    }
  end

  @impl true
  def to_map({_id, map}), do: map

  def compute_hash(id, prev, creator, hashes) do
    ([
       to_string(id),
       normalize(prev),
       to_string(creator)
     ] ++
       hashes)
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
