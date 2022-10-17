defmodule Ipncore.Channel do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.Repo
  alias __MODULE__

  @type t :: %__MODULE__{
          id: binary(),
          pubkey: binary(),
          enabled: boolean(),
          genesis_time: pos_integer(),
          last_height: pos_integer(),
          last_hash: binary(),
          block_count: pos_integer(),
          coins: pos_integer(),
          tx_count: pos_integer(),
          vsn: pos_integer(),
          created_at: pos_integer(),
          updated_at: pos_integer()
        }

  @regex Const.Regex.channel()
  @unit_time :millisecond

  @primary_key {:id, :string, []}
  schema "channel" do
    field(:pubkey, :binary)
    field(:enabled, :boolean, default: true)
    field(:genesis_time, :integer, default: 0)
    field(:last_height, :integer, default: 0)
    field(:last_hash, :binary)
    field(:block_count, :integer, default: 0)
    field(:coins, Ecto.Amount, default: 0)
    field(:tx_count, :integer, default: 0)
    field(:vsn, :integer, default: 0)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  def check_name?(channel_id), do: Regex.match?(@regex, channel_id)

  def new(%{
        "id" => id,
        "pubkey" => pubkey,
        "time" => time
      }) do
    if check_name?(id) do
      %Channel{
        id: id,
        pubkey: pubkey,
        created_at: time,
        updated_at: time
      }
    else
      throw(40230)
    end
  end

  def new(_), do: throw(40230)

  def get_group(channel_id) do
    channel_id
    |> String.split("-")
    |> List.first()
  end

  def put_genesis_time(channel_id, time) do
    now = :erlang.system_time(:millisecond)

    from(c in Channel, where: c.id == ^channel_id)
    |> Repo.update_all(
      [set: [genesis_time: time, updated_at: now]],
      prefix: "sys",
      returning: false
    )
  end

  def multi_insert(multi, name, channel) do
    channel_struct = new(channel)

    Ecto.Multi.insert(
      multi,
      name,
      channel_struct,
      returning: false,
      prefix: "sys"
    )
  end

  def multi_update(
        multi,
        name,
        channel_id,
        last_height,
        last_hash,
        total_coinbase,
        block_inc \\ 1,
        tx_inc \\ 1
      ) do
    time = :erlang.system_time(@unit_time)

    query = from(c in Channel, where: c.id == ^channel_id)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [
        set: [last_height: last_height, last_hash: last_hash, updated_at: time],
        inc: [block_count: block_inc, tx_count: tx_inc, coins: total_coinbase]
      ],
      returning: false,
      prefix: "sys"
    )
  end

  def multi_put_genesis_time(multi, _name, _channel_id, _block, false), do: multi

  def multi_put_genesis_time(multi, name, channel_id, block_time, true) do
    Ecto.Multi.run(multi, name, fn _repo, _ ->
      put_genesis_time(channel_id, block_time)

      {:ok, nil}
    end)
  end

  def get(channel_id) do
    from(c in Channel,
      where: c.id == ^channel_id and c.enabled,
      select: %{
        id: c.id,
        block_count: c.block_count,
        created_at: c.created_at,
        genesis_time: c.genesis_time,
        coins: c.coins,
        last_height: c.last_height,
        last_hash: c.last_hash,
        pubkey: fragment("encode(?, 'base64')", c.pubkey),
        tx_count: c.tx_count,
        vsn: c.vsn
      }
    )
    |> Repo.one(prefix: "sys")
  end

  def exists?(channel_id) do
    from(c in Channel, where: c.id == ^channel_id and c.enabled)
    |> Repo.exists?(prefix: "sys")
  end

  def all do
    from(c in Channel, where: c.enabled)
    |> Repo.all(prefix: "sys")
  end

  def all(params) do
    from(c in Channel, where: c.enabled)
    |> filter_index(params)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> sort(params)
    |> Repo.all(prefix: "sys")
  end

  defp filter_index(query, %{"id" => channel_id}) do
    where(query, [c], c.id == ^channel_id)
  end

  defp filter_index(query, _params), do: query

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [c], asc: c.created_at)

      _ ->
        order_by(query, [c], desc: c.created_at)
    end
  end
end
