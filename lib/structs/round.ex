defmodule Ippan.Round do
  alias Ippan.Block
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          creator: non_neg_integer(),
          signature: binary() | nil,
          coinbase: non_neg_integer(),
          count: non_neg_integer(),
          tx_count: non_neg_integer(),
          size: non_neg_integer(),
          reason: 0 | 1 | 2 | 3,
          blocks: [map()] | nil,
          extra: [any()] | nil
        }

  # Reason
  # 0 = Success
  # 1 = Timeout
  # 2 = Round data failure
  # 3 = Failure in all blocks of the round
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
    :reason,
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
      x.reason,
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
        reason,
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
      reason: reason,
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

  def from_remote(%{"blocks" => blocks} = msg_round) do
    blocks =
      Enum.reduce(blocks, [], fn b, acc ->
        block =
          MapUtil.to_atoms(b, Block.fields())

        acc ++ [block]
      end)

    msg_round
    |> MapUtil.to_atoms(~w(id creator hash prev signature))
    |> Map.put(:blocks, blocks)
  end

  def null?(%{reason: reason}) when reason > 0, do: true
  def null?(_), do: false

  @spec cancel(
          pos_integer(),
          binary() | nil,
          binary() | nil,
          binary() | nil,
          pos_integer(),
          pos_integer()
        ) :: map
  def cancel(id, hash, prev, signature, creator_id, reason) do
    %{
      id: id,
      hash: hash || prev,
      prev: prev,
      creator: creator_id,
      signature: signature,
      coinbase: 0,
      count: 0,
      tx_count: 0,
      size: 0,
      reason: reason,
      blocks: [],
      extra: nil
    }
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x
end
