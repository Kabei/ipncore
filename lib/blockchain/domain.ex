defmodule Ipncore.Domain do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Address, Database, DnsRecord, Repo, Tx}
  alias __MODULE__

  @behaviour Database

  @base :domain
  @filename "domain.db"
  # @fields ~w(name owner email avatar)
  @edit_fields ~w(enabled owner email avatar)
  @max_characters 25
  @token Default.token()
  @renewed_time 31_536_000_000
  @price_to_update 1000

  @primary_key {:name, :string, []}
  schema "domain" do
    field(:owner, :binary)
    field(:email, :string)
    field(:avatar, :string)
    field(:enabled, :boolean, default: true)
    field(:forsale, :boolean, default: false)
    field(:records, :integer, default: 0)
    field(:created_at, :integer)
    field(:renewed_at, :integer)
    field(:updated_at, :integer)
  end

  @impl Database
  def open do
    dir_path = Default.data_dir()
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, file: filename, keypos: :name, auto_save: 5_000)
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
        throw("Domain already exists")
    end
  end

  def put(x) do
    DetsPlus.insert(@base, x)
  end

  def exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        false

      true ->
        throw("Domain already exists")
    end
  end

  def not_exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        throw("Domain already exists")

      true ->
        true
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

      [_x] ->
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

      [_domain] ->
        throw("Invalid owner")

      _ ->
        throw("Domain not exists")
    end
  end

  def extract_root(domain) do
    domain
    |> String.split(".")
    |> Enum.take(-2)
    |> List.first()

    # |> Enum.join(".")
  end

  def check_new!(name, from_address, email, avatar, years_to_renew, validator_host, size) do
    if years_to_renew not in 1..2, do: throw("Invalid years to renew")
    if not Regex.match?(Const.Regex.hostname(), name), do: throw("Invalid name")
    if @max_characters < byte_size(name), do: throw("Max characters is 25")

    if not is_nil(email) and not Regex.match?(Const.Regex.email(), email),
      do: throw("Invalid email")

    if String.length(name) > 100, do: throw("Invalid name length")
    if !is_nil(avatar) and String.length(avatar) > 255, do: throw("Invalid avatar length")

    exists!(name)

    Tx.check_send!(
      from_address,
      Platform.address(),
      @token,
      price(name, years_to_renew),
      validator_host,
      size
    )

    :ok
  end

  def new!(
        multi,
        event_id,
        from_address,
        name,
        email,
        avatar,
        years_to_renew,
        validator_host,
        event_size,
        timestamp,
        channel
      ) do
    domain = %{
      name: name,
      email: email,
      avatar: avatar,
      enabled: true,
      forsale: false,
      records: 0,
      owner: from_address,
      created_at: timestamp,
      updated_at: timestamp,
      renewed_at: timestamp + years_to_renew * @renewed_time
    }

    put!(domain)

    Tx.send!(
      multi,
      event_id,
      @token,
      from_address,
      Platform.address(),
      price(name, years_to_renew),
      validator_host,
      event_size,
      nil,
      timestamp,
      channel
    )
    |> Ecto.Multi.insert_all(:domain, Domain, [domain],
      returning: false,
      prefix: channel
    )
  end

  def check_update!(name, from_address) do
    if is_nil(name), do: throw("No hostname")
    if not Regex.match?(Const.Regex.hostname(), name), do: throw("Invalid hostname")

    fetch!(name, from_address)
    Tx.check_fee!(from_address, @price_to_update)
    :ok
  end

  def event_update!(
        multi,
        event_id,
        name,
        from_address,
        validator_host,
        params,
        timestamp,
        channel
      ) do
    map_params =
      params
      |> MapUtil.require_only(@edit_fields)
      |> Map.take(@edit_fields)
      |> MapUtil.validate_length("avatar", 255)
      |> MapUtil.validate_email("email")
      |> MapUtil.validate_boolean("enabled")
      |> MapUtil.decode_address("owner")
      |> MapUtil.to_atoms()
      |> Map.put(:updated_at, timestamp)

    kw_params =
      map_params
      |> MapUtil.to_keywords()

    domain =
      fetch!(name, from_address)
      |> Map.merge(map_params)

    multi =
      Tx.send_fee!(
        multi,
        event_id,
        from_address,
        validator_host,
        @price_to_update,
        timestamp,
        channel
      )

    put(domain)

    queryable = from(d in Domain, where: d.name == ^name and d.owner == ^from_address)

    multi
    |> Ecto.Multi.update_all(:update, queryable, [set: kw_params],
      returning: false,
      prefix: channel
    )
  end

  def check_delete!(name, owner) do
    fetch!(name, owner)
  end

  def event_delete!(multi, name, owner, channel) do
    delete!(name, owner)

    queryable = from(d in Domain, where: d.name == ^name and d.owner == ^owner)

    multi
    |> Ecto.Multi.delete_all(:delete, queryable, prefix: channel)
    |> DnsRecord.delete_by_root(name, channel)
  end

  def count_records(%{records: records} = domain, count \\ 1) do
    %{domain | records: records + count}
    |> put()
  end

  def uncount_records(%{records: records} = domain, uncount \\ 1) do
    %{domain | records: records - uncount}
    |> put()
  end

  def exists?(name, channel) do
    from(d in Domain, where: d.enabled and d.name == ^name)
    |> Repo.exists?(prefix: channel)
  end

  defp price(name, years_to_renew) do
    x = String.length(name)

    cond do
      x <= 5 ->
        100_000

      x <= 8 ->
        75_000

      true ->
        5_000
    end
    |> Kernel.*(years_to_renew)
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
end
