defmodule Ipncore.Token do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3]
  import Ipnutils.Filters
  import Ipnutils.Macros, only: [deftypes: 1]
  alias Ipncore.Repo
  alias __MODULE__

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @data_fields ~w(id name creator decimals owner props)
  @edit_fields ~w(enabled name owner props)

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

  def new(
        %{
          "id" => token_id,
          "name" => name,
          "decimals" => decimals,
          "props" => %{"symbol" => _symbol} = props,
          "creator" => creator_address,
          "owner" => owner_address
        },
        time
      ) do
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

  def filter_data(params), do: Map.take(params, @data_fields)

  def fetch!(token_id, channel) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> Repo.one!(prefix: channel)
  end

  def fetch_and_check_delay(token_id, time, channel) do
    from(tk in Token,
      where: tk.id == ^token_id and tk.enabled and tk.updated_at + @delay_edit < ^time
    )
    |> Repo.one(prefix: channel)
  end

  def multi_insert(multi, name, token, time, channel) do
    token_struct = new(token, time)

    exists_symbol =
      from(tk in Token,
        where:
          fragment("?->>'symbol' = ?", tk.props, ^token_struct.props["symbol"]) and tk.enabled
      )
      |> Repo.exists?(prefix: channel)

    if exists_symbol do
      throw(40223)
    end

    Ecto.Multi.insert(
      multi,
      name,
      token_struct,
      returning: false,
      prefix: channel
    )
  end

  def multi_update(multi, name, token_id, params, time, channel) do
    query = from(tk in Token, where: tk.id == ^token_id and tk.updated_at + @delay_edit < ^time)

    params =
      params
      |> Map.take(@edit_fields)
      |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
      |> Keyword.put(:updated_at, time)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: params],
      returning: false,
      prefix: channel
    )
  end

  def multi_update_stats(multi, name, token_id, amount, time, channel) do
    query = from(tk in Token, where: tk.id == ^token_id)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: [updated_at: time], inc: [supply: amount]],
      returning: false,
      prefix: channel
    )
  end

  def get(token_id, channel) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select(nil)
    |> Repo.one(prefix: channel)
    |> transform_one()
  end

  def all(params) do
    from(tk in Token, where: tk.enabled)
    |> filter_index(params)
    |> filter_type(params)
    |> filter_group(params)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
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

  defp filter_select(query, %{"format" => "array"}) do
    select(query, [tk], [
      tk.id,
      tk.name,
      tk.type,
      tk.creator,
      tk.owner,
      tk.decimals,
      tk.supply,
      tk.props
    ])
  end

  defp filter_select(query, _params) do
    select(query, [tk], %{
      id: tk.id,
      name: tk.name,
      creator: tk.creator,
      owner: tk.owner,
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
    Enum.map(data, fn x -> transform_one(x) end)
  end

  defp transform_one([id, name, type, creator, owner, decimals, supply, props]),
    do: [
      id,
      name,
      type_name(type),
      Base58Check.encode(creator),
      Base58Check.encode(owner),
      decimals,
      supply,
      props
    ]

  defp transform_one(x),
    do: %{
      x
      | type: type_name(x.type),
        creator: Base58Check.encode(x.creator),
        owner: Base58Check.encode(x.owner)
    }
end
