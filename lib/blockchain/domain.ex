defmodule Ipncore.Domain do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Address, Database, Repo, Utils}
  alias __MODULE__

  @behaviour Database

  @base :domain
  @filename "domain.db"
  # @fields ~w(name owner email avatar)
  @edit_fields ~w(owner enabled email avatar)
  @token Default.token()

  @primary_key {:name, :string, []}
  schema "domain" do
    field(:owner, :binary)
    field(:email, :string)
    field(:avatar, :string)
    field(:enabled, :boolean, default: true)
    field(:records, :integer, default: 0)
    field(:created_at, :integer)
    field(:renewed_at, :integer)
    field(:updated_at, :integer)
  end

  @impl Database
  def open(_channel) do
    dir_path = Application.get_env(:ipncore, :dir_path)
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, name: filename, keypos: :name, auto_save: 60_000)
  end

  @impl Database
  def close(_channel) do
    DetsPlus.close(@base)
  end

  @impl Database
  def put!(x) do
    case DetsPlus.insert_new(@base, x) do
      true ->
        true

      false ->
        throw("Domain already exists")
    end
  end

  def put(x) do
    DetsPlus.insert(@base, x)
  end

  def exists!(x) do
    case DetsPlus.lookup(@base, x) do
      [] ->
        false

      _ ->
        throw("Domain already exists")
    end
  end

  @impl Database
  def fetch!(x) do
    case DetsPlus.lookup(@base, x) do
      [] ->
        throw("Domain not exists")

      [obj] ->
        obj
    end
  end

  def fetch!(name, owner) do
    DetsPlus.lookup(@base, name)
    |> case do
      [x] when x.owner == owner ->
        x

      [x] ->
        throw("Invalid owner")

      _ ->
        throw("Domain not exists")
    end
  end

  def delete!(name, owner) do
    case DetsPlus.lookup(@base, name) do
      [domain] when domain.owner == owner ->
        case DetsPlus.delete(@base, name) do
          {:error, _} -> throw("Error in the operation")
          r -> r
        end

      [domain] ->
        throw("Invalid owner")

      _ ->
        throw("Domain not exists")
    end
  end

  def new!(
        multi,
        channel,
        event_id,
        timestamp,
        event_size,
        from_address,
        name,
        owner,
        email,
        avatar,
        validator_address
      ) do
    if not Regex.match(Const.Regex.hostname(), name), do: throw("Invalid hostname")
    if !is_nil(email) and not Regex.match(Const.Regex.email(), email), do: throw("Invalid email")
    if String.length(name) > 100, do: throw("Invalid name length")

    domain = %{
      name: name,
      email: email,
      avatar: avatar,
      owner: owner,
      created_at: timestamp,
      updated_at: timestamp
    }

    exists!(name)

    multi =
      multi
      |> Tx.send_fee!(channel, event_id, timestamp, event_size, from_address, validator_address)
      |> Ecto.Multi.insert_all(:domain, Domain, [domain],
        returning: false,
        prefix: channel
      )

    put!(domain)

    multi
  end

  def event_update!(
        multi,
        channel,
        event_id,
        timestamp,
        event_size,
        owner,
        name,
        validator_address,
        params
      ) do
    if is_nil(name), do: throw("No hostname")

    kw_params =
      params
      |> Enum.take(@edit_fields)
      |> cast()
      |> Utils.to_keywords()
      |> Keyword.put(:updated_at, timestamp)

    domain =
      fetch!(name, owner)
      |> Map.merge(kw_params)

    queryable = from(d in Domain, where: d.name == ^name and d.owner == ^owner)

    multi =
      multi
      |> Tx.send_fees!(
        channel,
        event_id,
        timestamp,
        event_size,
        owner,
        @token,
        price(name),
        PlatformOwner.address(),
        validator_address,
        false,
        nil
      )
      |> Ecto.Multi.update_all(:update, queryable,
        set: kw_params,
        returning: false,
        prefix: channel
      )

    put(domain)

    multi
  end

  def event_delete!(multi, channel, owner, name) do
    delete!(name, owner)

    queryable = from(d in Domain, where: d.name == ^name and d.owner == ^owner)

    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel)
  end

  def exists?(name, channel) do
    from(d in Domain, where: d.enabled and d.name == ^name)
    |> Repo.exists?(prefix: channel)
  end

  defp price(name) do
    x = String.length(name)

    cond do
      x <= 5 ->
        100

      x <= 8 ->
        75

      true ->
        5
    end
  end

  def one(hostname, channel, params \\ %{}) do
    from(d in Domain, where: d.name == ^hostname and d.enabled)
    |> filter_select(params)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(d in Domain, where: d.enabled)
    |> filter_host(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_host(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [d], ilike(d.name, ^q))
  end

  defp filter_host(query, _), do: query

  defp filter_select(query, _) do
    select(query, [d], %{
      name: d.name,
      email: d.email,
      avatar: d.avatar,
      records: d.records,
      created_at: d.created_at,
      renewed_at: d.renewed_at,
      updated_at: d.updated_at
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
