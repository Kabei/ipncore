defmodule Ipncore.Token do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3]
  import Ipnutils.Filters
  import Ipnutils.Macros, only: [deftypes: 1]
  alias Ipncore.Repo
  alias __MODULE__

  @unit_time :millisecond
  # @channel Application.get_env(:ipncore, :channel)

  @type t :: %Token{
          id: binary(),
          name: binary(),
          type: integer(),
          group: binary(),
          decimals: integer(),
          enabled: boolean(),
          creator: binary(),
          owner: binary(),
          supply: pos_integer(),
          destroyed: pos_integer(),
          props: Map.t() | nil,
          created_at: pos_integer(),
          updated_at: pos_integer()
        }

  @schema_prefix "sys"
  @primary_key {:id, :binary, []}
  schema "token" do
    field(:name, :string)
    field(:type, :integer)
    field(:group, :string)
    field(:enabled, :boolean, default: true)
    field(:decimals, :integer)
    field(:creator, :binary)
    field(:owner, :binary)
    field(:supply, :integer, default: 0)
    field(:destroyed, :integer, default: 0)
    field(:props, :map)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  deftypes do
    [
      {0, "currency"},
      {1, "consumable"},
      {2, "unconsumable"},
      {3, "unique"},
      {4, "expirable"}
    ]
  else
    {0, "currency"}
  end

  def type_match(id) do
    cond do
      Regex.match?(Const.Regex.token_currency(), id) ->
        0

      Regex.match?(Const.Regex.token_consumable(), id) ->
        1

      Regex.match?(Const.Regex.token_unconsumable(), id) ->
        2

      Regex.match?(Const.Regex.token_unique(), id) ->
        3

      Regex.match?(Const.Regex.token_expirable(), id) ->
        4

      true ->
        0
    end
  end

  @spec coin?(token_id :: binary()) :: boolean()
  def coin?(token_id) do
    type_match(token_id) == 0
  end

  def product?(token_id) do
    type_match(token_id) in 1..4
  end

  def new(%{
        "id" => token_id,
        "name" => name,
        "decimals" => decimals,
        "props" => %{"symbol" => _symbol} = props,
        "creator" => creator_address,
        "owner" => owner_address,
        "time" => time
      })
      when is_integer(decimals) do
    %Token{
      id: token_id,
      name: name,
      props: props,
      decimals: decimals,
      type: type_match(token_id),
      creator: creator_address,
      owner: owner_address,
      created_at: time,
      updated_at: time
    }
  end

  def new(_), do: throw(40224)

  def get_pubkey(token_id, _channel) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled, select: tk.pubkey)
    |> Repo.one(prefix: @schema_prefix)
  end

  def multi_insert(multi, name, token, _channel) do
    token_struct = new(token)

    exists_symbol =
      from(tk in Token,
        where:
          fragment("?->>'symbol' = ?", tk.props, ^token_struct.props["symbol"]) and tk.enabled
      )
      |> Repo.exists?(prefix: @schema_prefix)

    if exists_symbol do
      throw(40223)
    end

    Ecto.Multi.insert(
      multi,
      name,
      token_struct,
      returning: false,
      prefix: @schema_prefix
    )
  end

  def multi_update(multi, name, token_id, amount, _channel) do
    time = :erlang.system_time(@unit_time)

    query = from(tk in Token, where: tk.id == ^token_id and tk.enabled)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: [updated_at: time], inc: [supply: amount]],
      returning: false,
      prefix: @schema_prefix
    )
  end

  def fetch(token_id) do
    Repo.get(__MODULE__, token_id, prefix: @schema_prefix)
  end

  def get(token_id) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select()
    |> Repo.one(prefix: @schema_prefix)
    |> transform()
  end

  def all(params) do
    from(tk in Token, where: tk.enabled)
    |> filter_index(params)
    |> filter_type(params)
    |> filter_group(params)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> filter_select()
    |> sort(params)
    |> Repo.all(prefix: @schema_prefix)
    |> filter_map()
  end

  defp filter_index(query, %{"id" => token_id}) do
    where(query, [tk], tk.id == ^token_id)
  end

  defp filter_index(query, _params), do: query

  defp filter_type(query, %{"type" => type}) do
    where(query, [tk], tk.type == ^type)
  end

  defp filter_type(query, _params), do: query

  defp filter_group(query, %{"group" => group}) do
    where(query, [tk], tk.group == ^group)
  end

  defp filter_group(query, _params), do: query

  defp filter_select(query) do
    select(query, [tk], %{
      id: tk.id,
      name: tk.name,
      decimals: tk.decimals,
      props: tk.props,
      type: tk.type,
      supply: tk.supply
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [tk], asc: tk.created_at)

      _ ->
        order_by(query, [tk], desc: tk.created_at)
    end
  end

  defp filter_map(data) do
    Enum.map(data, fn x -> transform(x) end)
  end

  defp transform(x), do: %{x | type: type_name(x.type)}
end
