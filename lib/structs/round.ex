defmodule Ippan.Round do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          creator: binary(),
          blocks: non_neg_integer(),
          timestamp: non_neg_integer()
        }

  defstruct [:id, :hash, :prev, :creator, :blocks, :timestamp]

  @impl true
  def to_list(x) do
    [x.id, x.hash, x.prev, x.creator, x.blocks, x.timestamp]
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
  def list_to_map([id, hash, prev, creator, blocks, timestamp]) do
    %{
      id: id,
      hash: hash,
      prev: prev,
      creator: creator,
      blocks: blocks,
      timestamp: timestamp
    }
  end

  @impl true
  def to_map({_id, map}), do: map

  def compute_hash(round, prev, creator, hashes) do
    ([
       to_string(round),
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
