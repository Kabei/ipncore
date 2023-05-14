defmodule Ipncore.Token do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3]
  import Ipnutils.Filters
  import Ipncore.Util
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
          avatar: String.t(),
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
  @edit_fields ~w(avatar name owner)
  @props ~w{maxSupply opts}
  @token Default.token()
  @max_decimals 18

  @primary_key {:id, :string, []}
  schema "token" do
    field(:name, :string)
    field(:enabled, :boolean, default: true)
    field(:decimals, :integer)
    field(:symbol, :string)
    field(:owner, :binary)
    field(:avatar, :string)
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

  def fetch(token_id) do
    case DetsPlus.lookup(@base, token_id) do
      [token] ->
        token

      _ ->
        nil
    end
  end

  def exists?(x) do
    DetsPlus.member?(@base, x)
  end

  def exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        false

      _ ->
        throw("Token already exists")
    end
  end

  def not_exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        throw("Token already not exists")

      _ ->
        true
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

  def check_new!(@token, _from_address) do
    if Platform.has_owner?(), do: throw("Token already exists")
    :ok
  end

  def check_new!(token_id, from_address) do
    if not coin?(token_id), do: throw("Invalid token ID")

    if not Platform.owner?(from_address),
      do: throw("Operation not allowed")

    exists!(token_id)
    :ok
  end

  def check_update!(token_id, from_address) do
    fetch!(token_id, from_address)
  end

  def check_delete!(token_id, from_address) do
    token = fetch!(token_id, from_address)

    if token.supply > 0 or token.burned > 0, do: throw("Invalid operation")

    :ok
  end

  def new!(
        multi,
        token_id,
        _from_address,
        owner,
        name,
        decimals,
        symbol,
        avatar,
        props,
        timestamp,
        channel
      ) do
    props =
      (props || %{})
      |> Map.take(@props)
      |> MapUtil.validate_not_empty()
      |> MapUtil.validate_value("maxSupply", :gt, 0)
      |> MapUtil.validate_any("opts", ["burn", "coinbase", "lock"])

    if decimals < 0 and decimals > @max_decimals, do: throw("Invalid decimals")
    if String.length(symbol) > 2, do: throw("Invalid symbol length")
    if String.length(avatar) > 255, do: throw("Invalid avatar length")

    token = %{
      id: token_id,
      name: name,
      owner: owner,
      decimals: decimals,
      symbol: symbol,
      avatar: avatar,
      props: props || %{},
      enabled: true,
      supply: 0,
      burned: 0,
      created_at: timestamp,
      updated_at: timestamp
    }

    put!(token)

    Platform.put(token)

    Ecto.Multi.insert_all(multi, :token, Token, [token],
      returning: false,
      prefix: channel
    )
  end

  def event_update!(multi, from_address, token_id, params, timestamp, channel)
      when is_map(params) do
    if empty?(token_id), do: throw("Bad format token ID")

    map_params =
      params
      |> Map.take(@edit_fields)
      |> MapUtil.validate_not_empty()
      |> MapUtil.validate_length("name", 100)
      |> MapUtil.validate_length("avatar", 255)
      |> MapUtil.validate_address("owner")
      |> MapUtil.to_atoms()
      |> Map.put(:updated_at, timestamp)

    kw_params =
      map_params
      |> MapUtil.to_keywords()

    token =
      fetch!(token_id, from_address)
      |> Map.merge(map_params)

    put(token)

    Platform.put(token)

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

  def check_max_supply!(token, amount) do
    max_supply = Token.get_props(token, "maxSupply", 0)
    new_supply = token.supply + amount
    if max_supply != 0 and new_supply > max_supply, do: throw("MaxSupply exceeded")

    new_supply
  end

  def put_supply(token, new_supply) do
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

  def check_opts(token, name) do
    name in Map.get(token.props || %{}, "opts", [])
  end

  def get_props(token, name, def) do
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

  def one(token_id, params \\ %{}) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  def all(params) do
    from(tk in Token, where: tk.enabled)
    |> filter_index(params)
    |> filter_search(params)
    |> filter_offset(params)
    |> filter_limit(params)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_index(query, %{"id" => token_id}) do
    where(query, [tk], tk.id == ^token_id)
  end

  defp filter_index(query, _params), do: query

  defp filter_search(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [tk], ilike(tk.id, ^q) or ilike(tk.name, ^q))
  end

  defp filter_search(query, _), do: query

  # defp filter_select(query, %{"fmt" => "array"}) do
  #   select(query, [tk], [
  #     tk.id,
  #     tk.name,
  #     tk.owner,
  #     tk.decimals,
  #     tk.supply,
  #     tk.avatar,
  #     tk.props
  #   ])
  # end

  defp filter_select(query, _params) do
    select(query, [tk], %{
      id: tk.id,
      name: tk.name,
      owner: tk.owner,
      avatar: tk.avatar,
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

  # defp transform([id, name, owner, decimals, supply, avatar, props]),
  #   do: [
  #     id,
  #     name,
  #     Address.to_text(owner),
  #     decimals,
  #     supply,
  #     avatar,
  #     props
  #   ]

  defp transform(x), do: %{x | owner: Address.to_text(x.owner)}
end
