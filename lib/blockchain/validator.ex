defmodule Ipncore.Validator do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Database, Block, Chain, Repo, Tx}
  alias __MODULE__

  @behaviour Database

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(owner host name fee fee_type)
  @edit_fields ~w(owner host name fee fee_type)

  @base :validator
  @filaname "validator.db"

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
  def open(_channel) do
    dir_path = Application.get_env(:ipncore, :dir_path)
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, name: filename, keypos: :host, auto_save: 60_000)
  end

  @impl Database
  def close(_channel) do
    DetsPlus.close(@base)
  end

  @impl Database
  def put!(validator) do
    case DetsPlus.insert_new(validator) do
      true ->
        true

      false ->
        throw("Validator already exists")
    end
  end

  @impl Database
  def fetch!(host) do
    case DetsPlus.lookup(@base, host) do
      nil ->
        throw("Validator no exists")

      [validator] ->
        validator
    end
  end

  def fetch!(host, owner) do
    result =
      DetsPlus.Ext.reduce_while(@base, nil, fn x, acc ->
        if x.host == host and x.owner == owner do
          {:halt, x}
        else
          {:cont, acc}
        end
      end)

    if is_nil(result), do: throw("Validator no exists")

    result
  end

  def delete!(key, address) do
    case DetsPlus.lookup(@base, key) do
      [validator] ->
        if validator.owner != address, do: throw("Invalid owner")
        DetsPlus.delete(@base, key)

      _ ->
        throw("Validator not exists")
    end
  end

  def new(channel, event, from_address, host, owner, name, fee, fee_type, multi)
      when fee_type >= 0 and fee_type <= 2 do
    if not Regex.match?(Const.Regex.hostname(), host), do: throw("Invalid hostname")
    if String.length(name) > 100, do: throw("Invalid name length")

    if from_address != PlatformOwner.address(),
      do: throw("Operation not allowed")

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
      prefix: cahnnel,
      returning: false
    )
  end

  def new(_) do
    throw("Validator.new not match")
  end

  def event_update!(channel, event, from_address, host, params) when is_map(params) do
    if is_nil(host), do: throw("No hostname")
    keywords = to_keywords(params, @edit_fields)
    if keywords == %{}, do: throw("Invalid parameters")

    fetch!(host, address)

    kw_params = Keyword.put(keywords, :updated_at, event.time)

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^address)

    multi
    |> Ecto.Multi.update_all(:update, queryable,
      set: kw_params,
      returning: false,
      prefix: channel
    )
  end

  def event_delete!(channel, event, address, host, multi) do
    delete!(host, address)

    queryable = from(v in Validator, where: v.host == ^host and v.owner == ^address)

    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel)
  end

  defp to_keywords(params, filter) do
    params
    |> Enum.take(filter)
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  # --------------------------------------------------
  def all(params) do
    from(p in Validator, where: p.enabled)
    |> filter_host(params)
    |> filter_select()
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_host(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [validator], ilike(validator.host, ^q) or ilike(validator.name, ^q))
  end

  defp filter_host(query, _), do: query

  defp filter_select(query) do
    select(query, [p], %{
      host: p.host,
      name: p.name,
      address: p.address,
      fee: p.fee,
      percent: p.percent,
      created_at: p.created_at,
      updated_at: p.updated_at
    })
  end

  defp filter_map(data) do
    Enum.map(data, fn x -> transform(x) end)
  end

  defp transform(x), do: %{x | address: Address.to_text(x.address)}
end
