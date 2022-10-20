defmodule Ipncore.Block do
  use Ecto.Schema
  import Ipnutils.Macros, only: [deftypes: 1]
  import Ecto.Query, only: [from: 1, from: 2, order_by: 3, select: 3]
  import Ipnutils.Filters
  alias Ipncore.{Chain, Repo, Tx}
  alias __MODULE__

  @version Default.version()
  @interval Dafault.interval()

  @block_type_genesis 0
  @block_type_regular 100

  @type block_type :: 0 | 100 | 200 | 201 | 300 | 400 | 401

  @type t :: %__MODULE__{
          index: pos_integer(),
          height: pos_integer(),
          prev: binary | nil,
          hash: binary,
          mk: binary,
          type: block_type(),
          time: pos_integer(),
          vsn: pos_integer(),
          tx_count: pos_integer(),
          amount: pos_integer(),
          txvol: pos_integer(),
          txs: [Tx.t()] | [] | nil
        }

  deftypes do
    [
      {0, "genesis"},
      {100, "regular"}
    ]
  else
    {false, false}
  end

  def version, do: @version

  @primary_key {:index, :integer, []}
  # @foreign_key_type :integer
  # @derive {Jason.Encoder, except: [:__meta__, :txs]}
  schema "block" do
    field(:height, :integer)
    field(:hash, :binary)
    field(:prev, :binary)
    field(:type, :integer, default: @block_type_regular)
    field(:mk, :binary)
    field(:time, :integer)
    field(:vsn, :integer, default: @version)
    field(:tx_count, :integer, default: 0)
    field(:amount, :integer, default: 0)
    field(:txvol, Ecto.Amount, default: 0)
    has_many(:txs, Tx, foreign_key: :block_index, references: :index)
  end

  defmacro map_select do
    quote do
      %{
        index: b.index,
        height: b.height,
        hash: b.hash,
        mk: b.mk,
        type: b.type,
        prev: b.prev,
        time: b.time,
        tx_count: b.tx_count,
        amount: b.amount,
        txvol: b.txvol,
        vsn: b.vsn
      }
    end
  end

  @spec first([Tx.t()]) :: t
  defp first(txs) do
    IO.inspect("Block first 1")
    {coinbase_amount, 0} = Block.sum_amounts(txs)

    time =
      Chain.get_time()
      |> format_block_time()

    %Block{
      index: 0,
      height: 0,
      prev: nil,
      time: time,
      tx_count: length(txs),
      type: @block_type_genesis,
      amount: coinbase_amount,
      txs: txs
    }
    |> put_merkle_root()
    |> put_hash()
  end

  @spec next(prev_block :: Block | nil, txs :: [] | [Tx.t()]) :: t() | nil
  def next(_, []), do: nil
  def next(nil, nil), do: nil
  def next(nil, txs), do: first(txs)

  def next(%Block{} = prev_block, txs) do
    IO.inspect("Block 1")
    IO.inspect(txs)
    {coinbase_amount, txvol} = Block.sum_amounts(txs)

    time =
      Chain.get_time()
      |> format_block_time()

    genesis_time = Chain.genesis_time()

    IO.inspect("Block 2")

    %Block{
      height: prev_block.height + 1,
      index: next_index(time, genesis_time),
      prev: prev_block.hash,
      tx_count: length(txs),
      txvol: txvol,
      amount: coinbase_amount,
      time: time,
      txs: txs
    }
    |> put_merkle_root()
    |> put_hash()
  end

  @doc """
  Format time to a perfect block interval time
  Example:
    input:  1650424800072 -> ~U[2022-04-20 03:20:00.072Z]
    result: 1650424800000 -> ~U[2022-04-20 03:20:00.000Z]
  """
  @spec format_block_time(block_time :: pos_integer()) :: block_time_formated :: pos_integer()
  def format_block_time(time), do: time - rem(time, @interval)

  def new(_prev_block, _index, []), do: nil
  def new(nil, 0, txs), do: first(txs)

  def new(%Block{} = prev_block, index, txs) do
    {coinbase_amount, txvol} = Block.sum_amounts(txs)

    time =
      Chain.get_time()
      |> format_block_time()

    %Block{
      index: index,
      height: prev_block.height + 1,
      prev: prev_block.hash,
      tx_count: length(txs),
      txvol: txvol,
      amount: coinbase_amount,
      time: time,
      txs: txs
    }
    |> put_merkle_root()
    |> put_hash()
  end

  @spec put_hash(t) :: t
  def put_hash(b) do
    %{b | hash: compute_hash(b)}
  end

  @spec compute_hash(t) :: binary
  def compute_hash(%{
        index: index,
        height: height,
        prev: prev,
        vsn: version,
        mk: mk
      }) do
    [
      :binary.encode_unsigned(version),
      channel(),
      :binary.encode_unsigned(height),
      :binary.encode_unsigned(index),
      Utils.normalize(prev),
      mk
    ]
    |> Crypto.hash3()
  end

  @spec put_merkle_root(Block.t()) :: Block.t()
  def put_merkle_root(b) do
    %{b | mk: compute_merkle_root(b)}
  end

  @spec compute_merkle_root(Block.t()) :: Block.t()
  def compute_merkle_root(%Block{} = b) do
    hashes = Enum.map(b.txs, & &1.hash)
    MerkleTree.root(hashes)
  end

  @spec sum_amounts([Tx.t()]) :: {coinbase :: pos_integer(), txvol :: pos_integer()}
  def sum_amounts(txs) do
    Enum.reduce(txs, {0, 0}, fn tx, {acc_coinbase, acc_total} ->
      if Tx.is_coinbase?(tx) do
        {tx.amount + acc_coinbase, acc_total}
      else
        {acc_coinbase, acc_total + tx.amount}
      end
    end)
  end

  def fetch_count(channel) do
    if Regex.match?(Const.Regex.channel(), channel) do
      %{rows: [[count]]} =
        Ecto.Adapters.SQL.query!(
          Ipncore.Repo,
          "SELECT n_live_tup FROM pg_stat_user_tables WHERE schemaname = $1 AND relname = $2 LIMIT 1",
          [channel, "block"]
        )

      count
    else
      0
    end
  end

  def fetch_genesis do
    from(b in Block, where: b.index == 0 and b.type == @block_type_genesis, limit: 1)
    |> Repo.one(prefix: Default.channel())
  end

  def fetch_last do
    from(b in Block, order_by: [desc: b.index], limit: 1)
    |> Repo.one(prefix: Default.channel())
  end

  @spec next_index() :: pos_integer()
  def next_index do
    iit = Chain.get_time()
    genesis_time = Chain.genesis_time()

    case genesis_time do
      0 ->
        0

      _ ->
        div(iit - genesis_time, @interval)
    end
  end

  @spec next_index(time :: pos_integer()) :: pos_integer()
  def next_index(time) do
    genesis_time = Chain.genesis_time()

    case genesis_time do
      0 ->
        0

      _ ->
        div(time - genesis_time, @interval)
    end
  end

  @spec next_index(pos_integer(), pos_integer()) :: pos_integer()
  def next_index(_timestamp, 0), do: 0

  def next_index(timestamp, genesis_time) do
    div(timestamp - genesis_time, @interval)
  end

  @spec block_index_start_time(pos_integer(), pos_integer()) :: pos_integer()
  def block_index_start_time(block_index, genesis_time) do
    block_index * @interval + genesis_time
  end

  def get(%{"index" => index} = params) do
    from(b in Block, where: b.index == ^index, select: map_select())
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform_one()
  end

  def get(%{"hash" => hash} = params) do
    from(b in Block, where: b.hash == ^hash, select: map_select())
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform_one()
  end

  def get(%{"height" => height} = params) do
    from(b in Block, where: b.height == ^height, select: map_select())
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform_one()
  end

  def get_by_index(index, channel) do
    from(b in Block, where: b.index == ^index, select: map_select())
    |> Repo.one(prefix: channel)
    |> transform_one()
  end

  def get_by_hash(hash, channel) do
    from(b in Block, where: b.hash == ^hash, select: map_select())
    |> Repo.one(prefix: channel)
    |> transform_one()
  end

  def get_by_height(height, channel) do
    from(b in Block, where: b.height == ^height, select: map_select())
    |> Repo.one(prefix: channel)
    |> transform_one()
  end

  def all(params) do
    from(Block)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> sort(params)
    |> filter_select(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [b], asc: b.index)

      _ ->
        order_by(query, [b], desc: b.index)
    end
  end

  defp filter_select(query, _params) do
    select(query, [b], map_select())
  end

  defp transform(nil), do: []
  defp transform([]), do: []

  defp transform(blocks) do
    Enum.map(blocks, &transform_one(&1))
  end

  defp transform_one(nil), do: nil

  defp transform_one(b) do
    %{
      b
      | hash: Base.encode16(b.hash, case: :lower),
        type: type_name(b.type),
        prev: prev_hash(b.prev),
        mk: Base.encode16(b.mk, case: :lower)
    }
  end

  defp prev_hash(nil), do: nil
  defp prev_hash(x), do: Base.encode16(x, case: :lower)

  def from_struct(b) do
    Map.drop(b, [:__meta__])
    |> Map.from_struct()
  end
end
