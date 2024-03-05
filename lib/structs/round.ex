defmodule Ippan.Round do
  alias Ippan.{Block, Utils}

  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hash: binary(),
          prev: binary() | nil,
          creator: non_neg_integer(),
          signature: binary() | nil,
          reward: non_neg_integer(),
          count: non_neg_integer(),
          tx_count: non_neg_integer(),
          size: non_neg_integer(),
          status: 0 | 1 | 2 | 3,
          timestamp: non_neg_integer(),
          blocks: [map()] | nil,
          extra: [any()] | nil
        }

  # Status
  # 0 = Success
  # 1 = Error timeout
  # 2 = Error round data failure
  # 3 = Failure in all blocks of the round
  defstruct [
    :id,
    :hash,
    :prev,
    :creator,
    :signature,
    :reward,
    :count,
    :tx_count,
    :size,
    :status,
    :timestamp,
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
      x.reward,
      x.count,
      x.tx_count,
      x.size,
      x.status,
      x.timestamp,
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
        reward,
        count,
        tx_count,
        size,
        status,
        timestamp,
        blocks,
        extra
      ]) do
    %{
      id: id,
      hash: hash,
      prev: prev,
      creator: creator,
      signature: signature,
      reward: reward,
      count: count,
      tx_count: tx_count,
      size: size,
      status: status,
      timestamp: timestamp,
      blocks: :erlang.element(1, CBOR.Decoder.decode(blocks)),
      extra: :erlang.element(1, CBOR.Decoder.decode(extra))
    }
  end

  @impl true
  def to_map({_id, map}), do: map

  def to_text(
        x = %{
          hash: hash,
          prev: prev,
          jackpot: {winner, amount},
          signature: signature,
          blocks: blocks
        }
      ) do
    blocks = Enum.map(blocks, &Block.to_text(&1))

    %{
      x
      | blocks: blocks,
        hash: Utils.encode16(hash),
        prev: Utils.encode16(prev),
        jackpot: %{winner: winner, amount: amount},
        signature: Utils.encode64(signature)
    }
  end

  def to_text(x = %{hash: hash, prev: prev, signature: signature}) do
    %{
      x
      | hash: Utils.encode16(hash),
        prev: Utils.encode16(prev),
        signature: Utils.encode64(signature)
    }
  end

  def to_text(x = %{hash: hash, prev: prev}) do
    %{x | hash: Utils.encode16(hash), prev: Utils.encode16(prev)}
  end

  def compute_hash(id, prev, creator, hashes, timestamp) do
    ([
       to_string(id),
       normalize(prev),
       to_string(creator),
       to_string(timestamp)
     ] ++
       hashes)
    |> IO.iodata_to_binary()
    |> Blake3.hash()
  end

  def calc_reward(0, _txs_rejected, _size), do: 5

  def calc_reward(txs_count, txs_rejected, size) do
    ((txs_count - txs_rejected) * (txs_count / size) * 100) |> trunc() |> max(5)
  end

  def from_remote(%{"blocks" => blocks} = msg_round) do
    blocks =
      Enum.reduce(blocks, [], fn b, acc ->
        block =
          MapUtil.to_atoms(b, Block.fields())

        acc ++ [block]
      end)

    msg_round
    # |> MapUtil.to_atoms(~w(id creator hash prev signature timestamp))
    |> MapUtil.to_atoms(~w(id creator hash prev signature size status timestamp tx_count))
    |> Map.put(:blocks, blocks)
  end

  def sync_remote(%{"blocks" => blocks} = msg_round) do
    blocks =
      Enum.reduce(blocks, [], fn b, acc ->
        block =
          MapUtil.to_atoms(b, Block.fields())

        acc ++ [block]
      end)

    msg_round
    |> MapUtil.to_atoms(
      ~w(id creator extra hash prev signature reward count size status timestamp tx_count)
    )
    |> Map.put(:blocks, blocks)
  end

  def is_some_block_mine?([], _vid), do: false

  def is_some_block_mine?(blocks, vid) do
    Enum.any?(blocks, &(&1.creator == vid))
  end

  def null?(%{status: status}) when status > 0, do: true
  def null?(_), do: false

  # status: 1 = timeout
  # status: 2 = Error all block with error
  @spec cancel(
          non_neg_integer(),
          binary() | nil,
          non_neg_integer(),
          non_neg_integer()
        ) :: map
  def cancel(id, prev, creator_id, status) when status > 0 do
    hash = compute_hash(id, prev, creator_id, [], 0)

    %{
      id: id,
      hash: hash,
      prev: prev,
      creator: creator_id,
      signature: nil,
      reward: 0,
      count: 0,
      tx_count: 0,
      size: 0,
      status: status,
      timestamp: 0,
      blocks: [],
      extra: nil
    }
  end

  defp normalize(nil), do: ""
  defp normalize(x), do: x

  defmacro exists?(id) do
    quote bind_quoted: [id: id], location: :keep do
      Sqlite.exists?("exists_round", [id])
    end
  end

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_round", unquote(args))
    end
  end

  defmacro get(id) do
    quote bind_quoted: [id: id], location: :keep do
      Sqlite.fetch("get_round", [id])
      |> case do
        nil -> nil
        x -> Ippan.Round.list_to_map(x)
      end
    end
  end

  defmacro last do
    quote location: :keep do
      Sqlite.fetch("last_round", [])
      |> case do
        nil -> %{hash: nil, id: -1}
        x -> Ippan.Round.list_to_map(x)
      end
    end
  end

  defmacro last_created(creator_id, default \\ nil) do
    quote bind_quoted: [id: creator_id, default: default], location: :keep do
      Sqlite.fetch("last_round_by_creator", [id])
      |> case do
        nil -> default
        x -> Ippan.Round.list_to_map(x)
      end
    end
  end

  defmacro fetch_all(starts, limit, offset) do
    quote bind_quoted: [starts: starts, limit: limit, offset: offset], location: :keep do
      Sqlite.fetch_all("get_rounds", [starts, limit, offset])
      |> case do
        nil -> []
        data -> Enum.map(data, fn x -> Ippan.Round.list_to_map(x) end)
      end
    end
  end
end
