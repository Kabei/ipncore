defmodule Ipncore.Token do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3]
  import Ipnutils.Filters
  import Ipnutils.Macros, only: [deftypes: 1]
  alias Ipncore.{Block, Chain, Tx, TxData, Repo}
  alias __MODULE__

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(id name creator decimals symbol owner props)
  @edit_fields ~w(enabled name owner props)

  @type t :: %Token{
          id: binary(),
          name: binary(),
          type: integer(),
          symbol: binary(),
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
    field(:enabled, :boolean, default: true)
    field(:decimals, :integer)
    field(:symbol, :string)
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
          "symbol" => symbol,
          "creator" => creator_address,
          "owner" => owner_address
        } = params,
        time
      ) do
    %Token{
      id: token_id,
      name: name,
      props: params["props"],
      decimals: decimals,
      symbol: symbol,
      type: type_match(token_id),
      creator: creator_address,
      owner: owner_address,
      created_at: time,
      updated_at: time
    }
  end

  def new(_), do: throw(40224)

  def filter_data(params), do: Map.take(params, @fields)

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
      from(tk in Token, where: tk.symbol == ^token_struct.symbol)
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
      |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
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

  def get(token_id, channel, params \\ %{}) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: channel)
    |> transform_one()
  end

  def get(token_id, channel, params \\ %{}) do
    from(tk in Token, where: tk.id == ^token_id and tk.enabled)
    |> filter_select(params)
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

  defp filter_select(query, %{"fmt" => "array"}) do
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

  def processing(%{
        "channel" => channel_id,
        "sig" => sig64,
        "data" =>
          %{
            "id" => token_id,
            "name" => _token_name,
            "decimals" => token_decimal,
            "creator" => creator58,
            "owner" => owner58,
            "props" => %{
              "symbol" => token_symbol
            }
          } = token,
        "time" => time,
        "type" => "token_new" = type_name,
        "version" => version
      }) do
    # check token ID format
    unless coin?(token_id), do: throw(40222)

    # check token-symbol is string valid
    unless String.valid?(token_symbol), do: throw(40233)

    # check token-decimal no max 10
    unless is_integer(token_decimal) or token_decimal > 10 or token_decimal < 0,
      do: throw(40234)

    next_index = Block.next_index(time)
    genesis_time = Chain.genesis_time()
    creator = Base58Check.decode(creator58)
    owner = Base58Check.decode(owner58)
    signature = Base.decode64!(sig64)

    token =
      token
      |> Map.put("creator", creator)
      |> Map.put("owner", owner)

    data =
      token
      |> filter_data()
      |> CBOR.encode()

    tx =
      %{
        block_index: next_index,
        data: data,
        sigs: [signature],
        time: time,
        type: type,
        status: @status_approved,
        vsn: version
      }
      |> Tx.put_hash(data)
      |> Tx.put_index(genesis_time)
      |> Tx.put_size(data)
      |> Tx.verify_sign(signature, PlatformOwner.pubkey())

    Ecto.Multi.new()
    |> Ecto.Multi.insert(:tx, tx, returning: false, prefix: channel_id)
    |> TxData.multi_insert(:txdata, tx.index, data, TxData.cbor_mime(), channel_id)
    |> multi_insert(:token, token, time, channel_id)
    |> Repo.transaction()
    |> case do
      {:ok, _} ->
        {:ok, tx}

      err ->
        IO.inspect(err)
        {:error, 500}
    end
  end
end
