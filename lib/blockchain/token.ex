defmodule Ipncore.Token do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3]
  import Ipnutils.Filters
  import Ipnutils.Macros, only: [deftypes: 1]
  alias Ipncore.{Address, Repo, Database}
  alias __MODULE__

  @type t :: %Token{
          id: String.t(),
          name: String.t(),
          decimals: integer(),
          symbol: String.t(),
          enabled: boolean(),
          owner: binary(),
          supply: pos_integer(),
          burned: pos_integer(),
          props: Map.t() | nil,
          created_at: pos_integer(),
          updated_at: pos_integer()
        }

  @behaviour Database

  @base :token
  @filename "token.db"
  # @fields ~w(id name creator decimals symbol owner props)
  @edit_fields ~w(name owner)
  # @props ~w{maxSupply allowBlock allowBurn}

  @primary_key {:id, :string, []}
  schema "token" do
    field(:name, :string)
    field(:enabled, :boolean, default: true)
    field(:decimals, :integer)
    field(:symbol, :string)
    field(:owner, :binary)
    field(:supply, :integer, default: 0)
    field(:burned, :integer, default: 0)
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

  @impl Database
  def open do
    dir_path = Default.data_dir()
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, file: filename, keypos: :id, auto_save: 60_000)
  end

  @impl Database
  def close do
    DetsPlus.close(@base)
  end

  @impl Database
  def put!(x) do
    case DetsPlus.insert_new(@base, x) do
      true ->
        true

      false ->
        throw("Token already exists")
    end
  end

  def put(x) do
    DetsPlus.insert(@base, x)
  end

  @impl Database
  def fetch!(token_id) do
    case DetsPlus.lookup(@base, token_id) do
      [] ->
        throw("Token not exists")

      [token] ->
        token
    end
  end

  def fetch!(token_id, owner) do
    DetsPlus.lookup(@base, token_id)
    |> case do
      [x] when x.owner == owner ->
        x

      [_x] ->
        throw("Invalid owner")

      _ ->
        throw("Token not exists")
    end
  end

  def exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        false

      _ ->
        throw("Token already exists")
    end
  end

  def delete!(token_id, owner) do
    case DetsPlus.lookup(@base, token_id) do
      [x] when x.owner == owner ->
        case DetsPlus.delete(@base, token_id) do
          {:error, _} -> throw("Error in the operation")
          r -> r
        end

      [_x] ->
        throw("Invalid owner")

      _ ->
        throw("Token not exists")
    end
  end

  def check_new!(token_id, from_address) do
    if not coin?(token_id), do: throw("Invalid token ID")

    if from_address != PlatformOwner.address(),
      do: throw("Operation not allowed")

    exists!(token_id)
  end

  def check_delete!(token_id, from_address) do
    fetch!(token_id, from_address)
  end

  def new!(
        multi,
        token_id,
        _from_address,
        owner,
        name,
        decimals,
        symbol,
        props,
        timestamp,
        channel
      ) do
    props =
      (props || %{})
      |> MapUtil.validate_value("maxSupply", :lte, 0)
      |> MapUtil.validate_boolean("allowBlock")
      |> MapUtil.validate_boolean("allowBurn")

    token = %{
      id: token_id,
      name: name,
      owner: owner,
      decimals: decimals,
      symbol: symbol,
      props: props || %{},
      enabled: true,
      supply: 0,
      burned: 0,
      created_at: timestamp,
      updated_at: timestamp
    }

    put!(token)

    Ecto.Multi.insert_all(multi, :token, Token, [token],
      returning: false,
      prefix: channel
    )
  end

  def event_update!(multi, from_address, token_id, params, timestamp, channel)
      when is_map(params) do
    if is_nil(token_id), do: throw("Bad format token ID")

    token_params =
      params
      |> MapUtil.require_only(@edit_fields)
      |> Map.take(@edit_fields)
      |> MapUtil.validate_length("name", 100)
      |> MapUtil.validate_address("owner")

    kw_params =
      token_params
      |> MapUtil.to_atom_keywords()
      |> Map.put(:updated_at, timestamp)

    fetch!(token_id, from_address)
    |> Map.merge(token_params)
    |> put()

    queryable = from(tk in Token, where: tk.id == ^token_id and tk.owner == ^from_address)

    Ecto.Multi.update_all(multi, :update, queryable, [set: kw_params],
      returning: false,
      prefix: channel
    )
  end

  def event_delete!(multi, token_id, owner, channel) do
    delete!(token_id, owner)

    queryable = from(tk in Token, where: tk.id == ^token_id and tk.owner == ^owner)

    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel)
  end

  def check_supply!(token, amount) do
    max_supply = Token.get_param(token, "maxSupply", 0)
    supply = token.supply + amount
    if max_supply != 0 and supply > max_supply, do: throw("MaxSupply exceeded")

    supply
  end

  def update_supply(token, new_supply) do
    %{token | supply: new_supply}
    |> put()
  end

  def multi_update_stats(multi, name, token_id, inc_params, time, channel) do
    query = from(tk in Token, where: tk.id == ^token_id)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: [updated_at: time], inc: inc_params],
      returning: false,
      prefix: channel
    )
  end

  def get_param(token, name, def) do
    Map.get(token.props || %{}, name, def)
  end

  @spec owner?(String.t(), binary) :: boolean
  def owner?(token_id, owner) do
    DetsPlus.lookup(@base, token_id)
    |> case do
      [x] ->
        x.owner == owner

      _ ->
        false
    end
  end

  def one(token_id, channel, params \\ %{}) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(tk in Token, where: tk.enabled)
    |> filter_index(params)
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

  defp filter_select(query, %{"fmt" => "array"}) do
    select(query, [tk], [
      tk.id,
      tk.name,
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
      owner: tk.owner,
      decimals: tk.decimals,
      symbol: tk.symbol,
      supply: tk.supply,
      props: tk.props
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

  defp transform(nil), do: nil

  defp transform([id, name, owner, decimals, supply, props]),
    do: [
      id,
      name,
      Address.to_text(owner),
      decimals,
      supply,
      props
    ]

  defp transform(x), do: %{x | owner: Address.to_text(x.owner)}
end
