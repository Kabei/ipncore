defmodule Ipncore.Block do
  use Ecto.Schema
  import Ipnutils.Macros, only: [deftypes: 1]
  import Ecto.Query, only: [from: 1, from: 2, order_by: 3, select: 3]
  import Ipnutils.Filters
  alias Ipncore.{Event, Repo, Tx}
  alias __MODULE__

  @version Default.version()
  @interval Default.block_interval()

  @block_type_genesis 0
  @block_type_regular 1
  @epoch Application.compile_env(:ipncore, :epoch)

  @type block_type :: 0 | 100 | 200 | 201 | 300 | 400 | 401

  @type t :: %__MODULE__{
          height: pos_integer(),
          prev: binary | nil,
          hash: binary,
          mk: binary,
          type: block_type(),
          time: pos_integer(),
          vsn: pos_integer(),
          ev_count: pos_integer(),
          events: [Event.t()] | [] | nil
        }

  deftypes do
    [
      {0, "genesis"},
      {1, "regular"}
    ]
  else
    {false, false}
  end

  def version, do: @version

  @primary_key {:height, :integer, []}
  schema "block" do
    field(:hash, :binary)
    field(:prev, :binary)
    field(:type, :integer, default: @block_type_regular)
    field(:mk, :binary)
    field(:time, :integer)
    field(:vsn, :integer, default: @version)
    field(:ev_count, :integer, default: 0)
    has_many(:events, Event, foreign_key: :block_index, references: :height)
  end

  defmacro map_select do
    quote do
      %{
        height: b.height,
        hash: b.hash,
        mk: b.mk,
        type: b.type,
        prev: b.prev,
        time: b.time,
        ev_count: b.ev_count,
        vsn: b.vsn
      }
    end
  end

  @spec first([Tx.t()]) :: t
  defp first(events) do
    IO.inspect("Block first")

    time =
      :os.system_time(:millisecond)
      |> format_block_time()

    %Block{
      height: 0,
      prev: nil,
      time: time,
      ev_count: length(events),
      type: @block_type_genesis,
      events: events
    }
    |> put_merkle_root()
    |> put_hash()
  end

  @spec next(prev_block :: Block.t() | nil, events :: [] | [Tx.t()]) :: t() | nil
  def next(_, []), do: nil
  def next(nil, nil), do: nil
  def next(nil, events), do: first(events)

  def next(prev_block, events) do
    IO.inspect("Block #{prev_block.height + 1}")

    time =
      :os.system_time(:millisecond)
      |> format_block_time()

    %Block{
      height: prev_block.height + 1,
      prev: prev_block.hash,
      ev_count: length(events),
      time: time,
      type: @block_type_regular,
      events: events
    }
    |> put_merkle_root()
    |> put_hash()
  end

  @spec epoch(block_height :: pos_integer() | nil) :: pos_integer()
  def epoch(nil), do: 0
  def epoch(0), do: 0
  def epoch(block_height), do: div(block_height, @epoch)

  @doc """
  Format time to a perfect block interval time
  Example:
    input:  1650424800072 -> ~U[2022-04-20 03:20:00.072Z]
    result: 1650424800000 -> ~U[2022-04-20 03:20:00.000Z]
  """
  @spec format_block_time(block_time :: pos_integer()) :: block_time_formated :: pos_integer()
  def format_block_time(time), do: time - rem(time, @interval)

  # def new(_prev_block, []), do: nil
  # def new(nil, txs), do: first(txs)

  # def new(%Block{} = prev_block, events) do
  #   time =
  #     Chain.get_time()
  #     |> format_block_time()

  #   %Block{
  #     height: prev_block.height + 1,
  #     prev: prev_block.hash,
  #     ev_count: length(events),
  #     time: time,
  #     events: events
  #   }
  #   |> put_merkle_root()
  #   |> put_hash()
  # end

  @spec put_hash(t) :: t
  def put_hash(b) do
    %{b | hash: compute_hash(b)}
  end

  @spec compute_hash(t) :: binary
  def compute_hash(%{
        height: height,
        prev: prev,
        vsn: version,
        mk: mk
      }) do
    [
      to_string(version),
      to_string(height),
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
    hashes = Enum.map(b.events, & &1.hash)
    MerkleTree.root(hashes)
  end

  # def fetch_count(channel) do
  #   if Regex.match?(Const.Regex.channel(), channel) do
  #     %{rows: [[count]]} =
  #       Ecto.Adapters.SQL.query!(
  #         Ipncore.Repo,
  #         "SELECT n_live_tup FROM pg_stat_user_tables WHERE schemaname = $1 AND relname = $2 LIMIT 1",
  #         [channel, "block"]
  #       )

  #     count
  #   else
  #     0
  #   end
  # end

  def fetch_genesis do
    from(b in Block, where: b.height == 0 and b.type == @block_type_genesis, limit: 1)
    |> Repo.one(prefix: Default.channel())
  end

  def fetch_last do
    from(b in Block, order_by: [desc: b.height], limit: 1)
    |> Repo.one(prefix: Default.channel())
  end

  # @spec next_index() :: pos_integer()
  # def next_index do
  #   iit = Chain.get_time()
  #   genesis_time = Chain.genesis_time()

  #   case genesis_time do
  #     0 ->
  #       0

  #     _ ->
  #       div(iit - genesis_time, @interval)
  #   end
  # end

  # @spec next_index(time :: pos_integer()) :: pos_integer()
  # def next_index(time) do
  #   genesis_time = Chain.genesis_time()

  #   case genesis_time do
  #     0 ->
  #       0

  #     _ ->
  #       div(time - genesis_time, @interval)
  #   end
  # end

  # @spec next_index(pos_integer(), pos_integer()) :: pos_integer()
  # def next_index(_timestamp, 0), do: 0

  # def next_index(timestamp, genesis_time) do
  #   div(timestamp - genesis_time, @interval)
  # end

  # @spec block_index_start_time(pos_integer(), pos_integer()) :: pos_integer()
  # def block_index_start_time(block_index, genesis_time) do
  #   block_index * @interval + genesis_time
  # end

  # def get(%{"height" => height} = params) do
  #   from(b in Block, where: b.height == ^height, select: map_select())
  #   |> Repo.one(prefix: filter_channel(params, Default.channel()))
  #   |> transform()
  # end

  def get(%{"hash" => hash} = params) do
    from(b in Block, where: b.hash == ^hash, select: map_select())
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  def get(%{"height" => height} = params) do
    from(b in Block, where: b.height == ^height, select: map_select())
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  def get_by_hash(hash, channel) do
    from(b in Block, where: b.hash == ^hash, select: map_select())
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def get_by_height(height, channel) do
    from(b in Block, where: b.height == ^height, select: map_select())
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(Block)
    |> filter_offset(params)
    |> filter_limit(params)
    |> sort(params)
    |> filter_select(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [b], asc: b.height)

      _ ->
        order_by(query, [b], desc: b.height)
    end
  end

  defp filter_select(query, _params) do
    select(query, [b], map_select())
  end

  defp filter_map(nil), do: []
  defp filter_map([]), do: []

  defp filter_map(blocks) do
    Enum.map(blocks, &transform(&1))
  end

  defp transform(nil), do: nil

  defp transform(b) do
    %{
      b
      | hash: Base.encode16(b.hash, case: :lower),
        type: type_name(b.type),
        prev: prev_hash(b.prev),
        mk: Base.encode16(b.mk, case: :lower)
    }
    |> Map.put(:epoch, epoch(b.height))
  end

  defp prev_hash(nil), do: nil
  defp prev_hash(x), do: Base.encode16(x, case: :lower)
end
