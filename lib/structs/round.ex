defmodule Ippan.Round do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          creator: non_neg_integer(),
          signature: binary(),
          coinbase: non_neg_integer(),
          count: non_neg_integer(),
          tx_count: non_neg_integer(),
          size: non_neg_integer(),
          blocks: [map()] | nil,
          extra: [any()] | nil
        }

  defstruct [
    :id,
    :hash,
    :prev,
    :creator,
    :signature,
    :coinbase,
    :count,
    :tx_count,
    :size,
    :blocks,
    :extra
  ]

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hash,
      x.prev,
      x.creator,
      x.signature,
      x.coinbase,
      x.count,
      x.tx_count,
      x.size,
      CBOR.encode(x.blocks),
      CBOR.encode(x.extra)
    ]
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
  def list_to_map([
        id,
        hash,
        prev,
        creator,
        signature,
        coinbase,
        count,
        tx_count,
        size,
        blocks,
        extra
      ]) do
    %{
      id: id,
      hash: hash,
      prev: prev,
      creator: creator,
      signature: signature,
      coinbase: coinbase,
      count: count,
      tx_count: tx_count,
      size: size,
      blocks: CBOR.Decoder.decode(blocks) |> elem(0),
      extra: CBOR.Decoder.decode(extra) |> elem(0)
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

  def reward(0, _txs_rejected, _size), do: 5

  def reward(txs_count, txs_rejected, size) do
    ((txs_count - txs_rejected) / size)
    |> Kernel.*(1000)
    |> trunc()
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
