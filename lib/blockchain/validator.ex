defmodule Ipncore.Validator do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  import Ipncore.Util
  alias Ipncore.{Address, Database, Repo}
  alias __MODULE__

  # @enforce_keys [:host, :owner]
  # defstruct host: nil,
  #           owner: nil,
  #           name: nil,
  #           avatar: nil,
  #           fee: 0,
  #           fee_type: 0,
  #           enabled: true,
  #           created_at: 0,
  #           updated_at: 0

  use DBTable,
    name: :validator,
    path: "validator",
    keypos: :host,
    auto_save: 5_000,
    shards: 8

  # @behaviour Database

  # Fee type
  # 0. by size
  # 1. percent
  # 2. fixed price

  @base :validator
  @filename "validator.db"
  @edit_fields ~w(name avatar owner fee fee_type)
  @max_name_length 100

  @primary_key {:host, :string, []}
  schema "validator" do
    field(:name, :string)
    field(:owner, :binary)
    field(:avatar, :string)
    field(:fee_type, :integer)
    field(:fee, :float)
    field(:enabled, :boolean, default: true)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  # @impl Database
  # def open do
  #   dir_path = Default.data_dir()
  #   filename = Path.join([dir_path, @filename])
  #   DetsPlus.open_file(@base, file: filename, keypos: :host, auto_save: 5_000)
  # end

  # @impl Database
  # def close do
  #   DetsPlus.close(@base)
  # end

  # @impl Database
  # def put!(x) do
  #   case DetsPlus.insert_new(@base, x) do
  #     true ->
  #       true

  #     false ->
  #       throw("Validator already exists")
  #   end
  # end

  # def put(x) do
  #   DetsPlus.insert(@base, x)
  # end

  # @impl Database
  # def fetch!(x) do
  #   case DetsPlus.lookup(@base, x) do
  #     [x] ->
  #       x

  #     _ ->
  #       throw("Validator not exists")
  #   end
  # end

  # def fetch!(host, owner) do
  #   case DetsPlus.lookup(@base, host) do
  #     [x] when x.owner == owner ->
  #       x

  #     [_x] ->
  #       throw("Invalid owner")

  #     _ ->
  #       throw("Validator not exists")
  #   end
  # end

  # def exists?(x) do
  #   DetsPlus.member?(@base, x)
  # end

  # def exists!(x) do
  #   case DetsPlus.lookup(@base, x) do
  #     [] ->
  #       false

  #     _ ->
  #       throw("Validator already exists")
  #   end
  # end

  # def delete!(key, owner) do
  #   case DetsPlus.lookup(@base, key) do
  #     [x] when x.owner == owner ->
  #       case DetsPlus.delete(@base, key) do
  #         {:error, _} -> throw("Error in the operation")
  #         r -> r
  #       end

  #     [_x] ->
  #       throw("Invalid owner")

  #     _ ->
  #       throw("Validator not exists")
  #   end
  # end

  def check_new!(host, name, from_address, avatar, fee_type, fee) do
    if not Regex.match?(Const.Regex.domain(), host), do: throw("Invalid domain")
    if String.length(name) > @max_name_length, do: throw("Invalid name length")
    if not Platform.owner?(from_address), do: throw("Operation not allowed")
    unless fee_type >= 0 and fee_type <= 2, do: throw("Invalid fee type")
    if not is_float(fee), do: throw("Invalid Fee value")
    if not empty?(avatar) and String.length(avatar) > 255, do: throw("Invalid avatar length")
    exists!(host)

    :ok
  end

  def new!(multi, _from_address, host, name, owner, avatar, fee_type, fee, timestamp, channel) do
    validator = %{
      host: host,
      name: name,
      owner: owner,
      avatar: avatar,
      fee_type: fee_type,
      fee: fee,
      created_at: timestamp,
      updated_at: timestamp
    }

    put_new!(validator)

    multi
    |> Ecto.Multi.insert_all(:validator, Validator, [validator],
      returning: false,
      prefix: channel
    )
  end

  def check_update!(host, from_address) do
    fetch_owner!(host, from_address)
  end

  def event_update!(multi, from_address, host, params, timestamp, channel) when is_map(params) do
    map_params =
      params
      |> Map.take(@edit_fields)
      |> MapUtil.validate_not_empty()
      |> MapUtil.validate_length("name", @max_name_length)
      |> MapUtil.validate_value("fee", :gt, 0)
      |> MapUtil.validate_range("fee_type", 0..2)
      |> MapUtil.validate_length("avatar", 255)
      |> MapUtil.validate_address("owner")
      |> MapUtil.to_atoms()
      |> Map.put(:updated_at, timestamp)

    kw_params =
      map_params
      |> MapUtil.to_keywords()

    fetch!(host, from_address)
    |> Map.merge(map_params)
    |> put()

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^from_address)

    Ecto.Multi.update_all(multi, :update, queryable, [set: kw_params],
      returning: false,
      prefix: channel
    )
  end

  def check_delete!(host, from_address) do
    fetch!(host, from_address)
  end

  def event_delete!(multi, host, owner, channel) do
    delete!(host, owner)

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^owner)

    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel)
  end

  def one(hostname, params \\ %{}) do
    channel = filter_channel(params, Default.channel())

    from(v in Validator, where: v.host == ^hostname and v.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(v in Validator, where: v.enabled)
    |> filter_search(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_search(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [v], ilike(v.host, ^q) or ilike(v.name, ^q))
  end

  defp filter_search(query, _), do: query

  defp filter_select(query, _) do
    select(query, [v], %{
      host: v.host,
      name: v.name,
      owner: v.owner,
      avatar: v.avatar,
      fee: v.fee,
      fee_type: v.fee_type,
      created_at: v.created_at,
      updated_at: v.updated_at
    })
  end

  defp filter_map(data) do
    Enum.map(data, fn x -> transform(x) end)
  end

  defp transform(nil), do: nil
  defp transform(x), do: %{x | owner: Address.to_text(x.owner)}
end
