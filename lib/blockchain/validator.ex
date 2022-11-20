defmodule Ipncore.Validator do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Address, Database, Block, Chain, Repo}
  alias __MODULE__

  @behaviour Database

  @base :validator
  @filename "validator.db"

  # @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @edit_fields ~w(owner name fee fee_type)

  @primary_key {:host, :string, []}
  schema "validator" do
    field(:name, :string)
    field(:owner, :binary)
    field(:fee, :float)
    field(:fee_type, :integer)
    field(:enabled, :boolean, default: true)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  @impl Database
  def open do
    dir_path = Application.get_env(:ipncore, :data_path, "data")
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, name: filename, keypos: :host, auto_save: 60_000)
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
        throw("Validator already exists")
    end
  end

  @impl Database
  def fetch!(x) do
    case DetsPlus.lookup(@base, x) do
      [] ->
        throw("Validator not exists")

      [x] ->
        x
    end
  end

  def fetch!(host, owner) do
    case DetsPlus.lookup(@base, host) do
      [x] when x.owner != owner ->
        x

      [_x] ->
        throw("Invalid owner")

      _ ->
        throw("Validator not exists")
    end
  end

  def exists!(x) do
    case DetsPlus.lookup(@base, x) do
      [] ->
        false

      _ ->
        throw("Validator already exists")
    end
  end

  def delete!(key, owner) do
    case DetsPlus.lookup(@base, key) do
      [x] when x.owner == owner ->
        case DetsPlus.delete(@base, key) do
          {:error, _} -> throw("Error in the operation")
          r -> r
        end

      [_x] ->
        throw("Invalid owner")

      _ ->
        throw("Validator not exists")
    end
  end

  def check_new!(host, name, from_address) do
    if not Regex.match?(Const.Regex.hostname(), host), do: throw("Invalid hostname")
    if String.length(name) > 100, do: throw("Invalid name length")
    if from_address != PlatformOwner.address(), do: throw("Operation not allowed")
    exists!(host)

    :ok
  end

  def new!(multi, _from_address, host, name, owner, fee, fee_type, channel)
      when fee_type >= 0 and fee_type <= 2 do
    validator = %{
      host: host,
      name: name,
      owner: owner,
      fee_type: fee_type,
      fee: fee
    }

    put!(validator)

    multi
    |> Ecto.Multi.insert_all(:validator, Validator, [validator],
      returning: false,
      prefix: channel
    )
  end

  def check_update!(name, from_address) do
    fetch!(name, from_address)
  end

  def event_update!(multi, from_address, host, params, timestamp, channel) when is_map(params) do
    atom_params =
      params
      |> MapUtil.require_only(@edit_fields)
      |> Map.take(@edit_fields)
      |> MapUtil.validate_length("name", 100)
      |> MapUtil.validate_value("fee", :gt, 0)
      |> MapUtil.validate_range("fee_type", 0..2)
      |> MapUtil.to_atoms()

    kw_params =
      atom_params
      |> Utils.to_keywords()
      |> Keyword.put(:updated_at, timestamp)

    validator = fetch!(host, from_address)
    Map.put(validator, :params, atom_params)
    put!(validator)

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^from_address)

    Ecto.Multi.update_all(multi, :update, queryable,
      set: kw_params,
      returning: false,
      prefix: channel
    )
  end

  def check_delete!(name, from_address) do
    fetch!(name, from_address)
  end

  def event_delete!(multi, owner, host, channel) do
    delete!(host, owner)

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^owner)

    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel)
  end

  def one(hostname, channel, params \\ %{}) do
    from(v in Validator, where: v.host == ^hostname and v.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(v in Validator, where: v.enabled)
    |> filter_host(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_host(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [v], ilike(v.host, ^q) or ilike(v.name, ^q))
  end

  defp filter_host(query, _), do: query

  defp filter_select(query, _) do
    select(query, [v], %{
      host: v.host,
      name: v.name,
      owner: v.owner,
      fee: v.fee,
      percent: v.percent,
      created_at: v.created_at,
      updated_at: v.updated_at
    })
  end

  defp filter_map(data) do
    Enum.map(data, fn x -> transform(x) end)
  end

  defp transform(nil), do: nil
  defp transform(x), do: %{x | owner: Address.to_text(x.owner)}

  defp cast(%{}), do: throw("Invalid parameters")
  defp cast(%{"owner" => address} = x), do: %{x | "owner" => Address.from_text(address)}
  defp cast(x), do: x
end
