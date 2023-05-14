defmodule Ipncore.Domain do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  import Ipncore.Util
  alias Ipncore.{Address, Database, DnsRecord, Repo, Tx}
  alias __MODULE__

  @behaviour Database

  @base :domain
  @filename "domain.db"
  # @fields ~w(name owner email avatar)
  @edit_fields ~w(enabled owner email avatar title)
  @max_characters 50
  @max_title_characters 64
  @max_avatar_characters 255
  @token Default.token()
  @renewed_time 31_536_000_000
  @max_renewed_time 63_072_000_000

  @price_to_update 1000

  @primary_key {:name, :string, []}
  schema "domain" do
    field(:owner, :binary)
    field(:title, :string)
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
    DetsPlus.open_file(@base, file: filename, keypos: :name, auto_save: 60_000)
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

  def exists?(x) do
    DetsPlus.member?(@base, x)
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

  @doc """
  Extract dopmain from hostname
  Example:
  iex> Domain.extract("sub.example.com")
  iex> "example.com"
  """
  def extract(hostname) do
    hostname
    |> String.split(".")
    |> Enum.take(-2)
    |> Enum.join(".")
  end

  @doc "Return subdomain and domain in a tuple from hostname or list hostname"
  def split(hostname_parts) when is_list(hostname_parts) do
    domain = Enum.take(hostname_parts, -2)
    subdomain = hostname_parts -- domain

    {Enum.join(subdomain, "."), Enum.join(domain, ".")}
  end

  def split(hostname) do
    parts = String.split(hostname, ".")

    domain = Enum.take(parts, -2)
    subdomain = parts -- domain

    {Enum.join(subdomain, "."), Enum.join(domain, ".")}
  end

  def check_new!(name, from_address, email, avatar, title, years_to_renew, validator_host, size) do
    if years_to_renew not in 1..2, do: throw("Invalid years to renew")
    if not Regex.match?(Const.Regex.ippan_domain(), name), do: throw("Invalid domain")
    if @max_characters < byte_size(name), do: throw("Max characters is 25")
    if @max_title_characters < String.length(title), do: throw("Max characters is 64")

    if not empty?(email) and not Regex.match?(Const.Regex.email(), email),
      do: throw("Invalid email")

    if String.length(name) > 100, do: throw("Invalid name length")

    if not empty?(avatar) and String.length(avatar) > @max_avatar_characters,
      do: throw("Invalid avatar length")

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
        title,
        years_to_renew,
        validator_host,
        event_size,
        timestamp,
        channel
      ) do
    domain = %{
      name: name,
      title: title,
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
      false,
      timestamp,
      channel
    )
    |> Ecto.Multi.insert_all(:domain, Domain, [domain],
      returning: false,
      prefix: channel
    )
  end

  def check_update!(name, from_address) do
    if empty?(name) or not Regex.match?(Const.Regex.ippan_domain(), name),
      do: throw("Invalid domain")

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
      |> Map.take(@edit_fields)
      |> MapUtil.validate_not_empty()
      |> MapUtil.validate_length("title", 64)
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
    |> DnsRecord.delete_by_domain(name, channel)
  end

  def check_renew!(name, from_address, years_to_renew, validator_host, timestamp, size) do
    if empty?(name) or not Regex.match?(Const.Regex.ippan_domain(), name),
      do: throw("Invalid domain")

    domain = fetch!(name, from_address)

    if timestamp > domain.renewed_at, do: throw("Domain renewal invalid")

    renew = :timer.hours(24) * 365 * years_to_renew
    result = domain.renewed_at + renew - timestamp
    if result > @max_renewed_time, do: throw("Renewal exceeded")

    Tx.check_send!(
      from_address,
      Platform.address(),
      @token,
      priceRenew(years_to_renew),
      validator_host,
      size
    )

    :ok
  end

  def event_renew!(
        multi,
        event_id,
        from_address,
        name,
        years_to_renew,
        validator_host,
        event_size,
        timestamp,
        channel
      ) do
    domain = fetch!(name, from_address)
    renew = :timer.hours(24) * 365 * years_to_renew
    result = domain.renewed_at + renew - timestamp
    if result > @max_renewed_time, do: throw("Renewal exceeded")

    new_value = domain.renewed_at + renew

    multi =
      Tx.send!(
        multi,
        event_id,
        @token,
        from_address,
        Platform.address(),
        priceRenew(years_to_renew),
        validator_host,
        event_size,
        nil,
        false,
        timestamp,
        channel
      )

    put(%{domain | renewed_at: new_value, updated_at: timestamp})

    queryable = from(d in Domain, where: d.name == ^name and d.owner == ^from_address)

    multi
    |> Ecto.Multi.update_all(
      :update,
      queryable,
      [set: [renewed_at: new_value, updated_at: timestamp]],
      returning: false,
      prefix: channel
    )
  end

  def count_records(multi, %{name: name, records: records} = domain, channel_id, count \\ 1) do
    %{domain | records: records + count}
    |> put()

    query = from(d in Domain, where: d.name == ^name)

    Ecto.Multi.update_all(multi, :records, query, [inc: [records: count]],
      returning: false,
      prefix: channel_id
    )
  end

  def uncount_records(multi, %{name: name, records: records} = domain, channel_id, uncount \\ 1) do
    %{domain | records: records - uncount}
    |> put()

    query = from(d in Domain, where: d.name == ^name)

    Ecto.Multi.update_all(multi, :records, query, [inc: [records: -uncount]],
      returning: false,
      prefix: channel_id
    )
  end

  def exists?(name, channel) do
    from(d in Domain, where: d.enabled and d.name == ^name)
    |> Repo.exists?(prefix: channel)
  end

  def price(name, years) do
    x =
      name
      |> String.split(".")
      |> List.first()
      |> String.length()

    base =
      cond do
        x <= 5 ->
          100_000

        x <= 8 ->
          75_000

        true ->
          5_000
      end

    base + (years - 1) * 5_000
    # throw("Invalid domain renewal year")
  end

  defp priceRenew(years) do
    years * 5_000
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
    |> filter_owner(params)
    |> filter_email(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_host(query, %{"name" => name}) do
    where(query, [d], d.name == ^name)
  end

  defp filter_host(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [d], ilike(d.name, ^q))
  end

  defp filter_host(query, _), do: query

  defp filter_owner(query, %{"owner" => owner}) do
    bin_owner = Address.from_text(owner)
    where(query, [d], d.owner == ^bin_owner)
  end

  defp filter_owner(query, _), do: query

  defp filter_email(query, %{"email" => email}) do
    where(query, [d], d.email == ^email)
  end

  defp filter_email(query, _), do: query

  defp filter_select(query, _) do
    select(query, [d], %{
      name: d.name,
      title: d.title,
      email: d.email,
      avatar: d.avatar,
      owner: d.owner,
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
