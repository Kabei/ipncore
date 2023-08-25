defmodule Ippan.Round do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          blocks: non_neg_integer(),
          timestamp: non_neg_integer(),
          vsn: pos_integer()
        }

  @blockchain_version Application.compile_env(:ipncore, :verison) |> to_string()

  defstruct [:id, :hash, :prev, :blocks, :timestamp, :vsn]

  def to_list(x) do
    [x.id, x.hash, x.prev, x.blocks, x.timestamp, x.vsn]
  end

  def to_tuple(x) do
    {x.id, x.hash, x.prev, x.blocks, x.timestamp, x.vsn}
  end

  def to_map({id, hash, prev, blocks, timestamp, vsn}) do
    %{id: id, hash: hash, prev: prev, blocks: blocks, timestamp: timestamp, vsn: vsn}
  end

  def to_map([id, hash, prev, blocks, timestamp, vsn]) do
    %{id: id, hash: hash, prev: prev, blocks: blocks, timestamp: timestamp, vsn: vsn}
  end

  def compute_hash(round, prev, hashes) do
    ([
       to_string(round),
       normalize(prev),
       @blockchain_version
     ] ++
       hashes)
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
