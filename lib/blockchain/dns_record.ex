defmodule Ipncore.DnsRecord do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Domain, Repo, Tx}
  alias __MODULE__

  # @table :dns
  @base :dns
  @filename "dns.db"
  # @dns_types ~w(a ns cname soa ptr mx txt aaaa spf srv ds sshfp rrsig nsec dnskey caa uri hinfo wks)
  @dns_types ~w(A NS CNAME SOA PTR MX TXT AAAA SPF SRV DS SSHFP RRSIG NSEC DNSKEY CAA URI HINFO WKS)
  @max_domain_size 255
  @min_ttl 300
  @max_ttl 2_147_483_647
  @max_records 500
  @max_dns_record_items 10
  @max_bytes_size 255
  @price 500

  schema "dns_record" do
    field(:domain, :string)
    field(:name, :string)
    field(:type, :integer)
    field(:index, :integer, default: 0)
    field(:data, :string)
    field(:ttl, :integer, default: 3600)
  end

  def open do
    dir_path = Default.data_dir()
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, file: filename, auto_save: 5_000)
  end

  def close do
    DetsPlus.close(@base)
  end

  def put(x) do
    DetsPlus.insert(@base, x)
  end

  def lookup(domain) do
    DetsPlus.lookup(@base, domain)
  end

  @spec lookup(binary, binary, integer) :: list()
  def lookup(domain, subdomain, type) do
    DetsPlus.lookup(@base, {domain, subdomain, type})
  end

  def exists?(domain, subdomain, type), do: DetsPlus.member?(@base, {domain, subdomain, type})

  def check_new!(hostname, type, data, ttl, from_address)
      when is_binary(hostname) and is_binary(data) and ttl >= @min_ttl and ttl <= @max_ttl do
    hostname = String.downcase(hostname)
    if type not in @dns_types, do: throw("DNS record type not supported")
    if byte_size(hostname) > @max_domain_size, do: throw("DNS record domain size exceeded")
    if byte_size(data) > @max_bytes_size, do: throw("DNS record data size exceeded")
    if not Match.hostname?(hostname), do: throw("Invalid hostname")

    domain = Domain.extract(hostname)

    domain_map = Domain.fetch!(domain, from_address)
    if @max_records < domain_map.records, do: throw("Max record exceeded")

    Tx.check_fee!(from_address, @price)

    :ok
  end

  def event_new!(
        multi,
        event_id,
        from_address,
        hostname,
        type,
        data,
        ttl,
        validator_host,
        timestamp,
        channel
      ) do
    type_number = type_to_number(type)

    {subdomain, domain} = Domain.split(hostname)

    domain_map = Domain.fetch!(domain)

    {_, _, _, _, value} = :dnslib.resource('#{hostname} IN #{ttl} #{type} #{data}')

    multi =
      Tx.send_fee!(
        multi,
        event_id,
        from_address,
        validator_host,
        @price,
        timestamp,
        channel
      )

    key = {domain, subdomain, type_number}
    records = lookup(key)
    n = length(records)
    new_record = {value, ttl}
    put({key, records ++ [new_record]})

    multi = Domain.count_records(multi, domain_map, channel, 1)

    struct = %{
      domain: domain,
      name: subdomain,
      type: type_number,
      index: n,
      data: data,
      ttl: ttl
    }

    Ecto.Multi.insert_all(multi, :dns, DnsRecord, [struct], prefix: channel, returning: false)
  end

  def check_update!(hostname, type, index, data, ttl, from_address)
      when is_binary(hostname) and is_binary(data) and is_integer(index) and ttl >= @min_ttl and
             ttl <= @max_ttl do
    hostname = String.downcase(hostname)
    if type not in @dns_types, do: throw("DNS record type not supported")
    if byte_size(hostname) > @max_domain_size, do: throw("DNS record domain size exceeded")
    if byte_size(data) > @max_bytes_size, do: throw("DNS record data size exceeded")
    if not Match.hostname?(hostname), do: throw("Invalid hostname")

    domain = Domain.extract(hostname)

    domain_map = Domain.fetch!(domain, from_address)
    if @max_records < domain_map.records, do: throw("Max record exceeded")

    Tx.check_fee!(from_address, @price)

    :ok
  end

  def event_update!(
        multi,
        event_id,
        from_address,
        hostname,
        type,
        index,
        data,
        ttl,
        validator_host,
        timestamp,
        channel
      ) do
    type_number = type_to_number(type)

    {subdomain, domain} = Domain.split(hostname)

    {_, _, _, _, value} = :dnslib.resource('#{hostname} IN #{ttl} #{type} #{data}')

    multi =
      Tx.send_fee!(
        multi,
        event_id,
        from_address,
        validator_host,
        @price,
        timestamp,
        channel
      )

    key = {domain, subdomain, type_number}

    new_records =
      case lookup(key) do
        [] ->
          throw("Invalid no records")

        records ->
          n = length(records)
          if n + 1 > @max_dns_record_items, do: throw("Max record by type exceeded")
          if n < index, do: throw("Invalid record index")
          new_record = {value, ttl}
          List.update_at(records, index - 1, fn _ -> new_record end)
      end

    put({key, new_records})

    struct = %{
      domain: domain,
      name: subdomain,
      type: type_number,
      index: index,
      data: data,
      ttl: ttl
    }

    query =
      from(dr in DnsRecord,
        where:
          dr.domain == ^domain and dr.name == ^subdomain and dr.type == ^type_number and
            dr.index == ^index
      )

    Ecto.Multi.update_all(
      multi,
      :dns,
      query,
      [set: [data: struct.data, ttl: struct.ttl]],
      prefix: channel,
      returning: false
    )
  end

  def check_delete!([hostname, type, _index], from_address) do
    hostname = String.downcase(hostname)
    if type not in @dns_types, do: throw("DNS record type not supported")
    if not Match.hostname?(hostname), do: throw("Invalid hostname")

    {subdomain, domain} = Domain.split(hostname)
    Domain.fetch!(domain, from_address)
    if DetsPlus.member?(@base, {domain, subdomain, type}), do: throw("DNS record not exists")
  end

  def event_delete!(multi, [hostname, type, index], channel) do
    {subdomain, domain} = Domain.split(hostname)
    type_number = type_to_number(type)
    domain_map = Domain.fetch!(domain)

    key = {domain, subdomain, type}

    case lookup(key) do
      [] ->
        throw("No record to delete")

      records ->
        n = length(records)
        if n < index, do: throw("Invalid record index")

        if n == 1 do
          DetsPlus.delete(@base, key)
        else
          put({key, List.delete_at(records, index)})
        end
    end

    multi = Domain.uncount_records(multi, domain_map, channel, 1)

    query =
      from(dr in DnsRecord,
        where:
          dr.domain == ^domain and dr.name == ^subdomain and dr.type == ^type_number and
            dr.index == ^index
      )

    Ecto.Multi.delete_all(multi, :delete, query, prefix: channel, returning: false)
  end

  @doc "Delete all records that match the domain and return the number of the deleted record"
  def delete_by_domain(multi, domain, channel) do
    DetsPlus.reduce(@base, 0, fn {key, _records}, acc ->
      case key do
        {x, _, _} when x == domain ->
          DetsPlus.delete(@base, key)
          acc + 1

        _ ->
          acc
      end
    end)

    queryable = from(dr in DnsRecord, where: dr.domain == ^domain)
    Ecto.Multi.delete_all(multi, :delete_all, queryable, prefix: channel)
  end

  def one(domain, type, channel) do
    type_number = type_to_number(type)

    from(dr in DnsRecord, where: dr.domain == ^domain and dr.type == ^type_number)
    |> filter_select(nil)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(dr in DnsRecord)
    |> filter_domain(params)
    |> filter_name(params)
    |> filter_type(params)
    |> filter_data(params)
    |> filter_search(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform_list()
  end

  defp filter_domain(query, %{"domain" => domain}) do
    where(query, [dr], dr.domain == ^domain)
  end

  defp filter_domain(query, _), do: query

  defp filter_type(query, %{"type" => type}) do
    type_number = type_to_number(type)
    where(query, [dr], dr.type == ^type)
  end

  defp filter_type(query, _), do: query

  defp filter_name(query, %{"name" => name}) do
    search =
      if Match.hostname?(name) do
        {subdomain, _domain} = Domain.split(name)
        subdomain
      else
        name
      end
      |> String.downcase()

    where(query, [dr], dr.name == ^search)
  end

  defp filter_name(query, _), do: query

  defp filter_data(query, %{"data" => data}) do
    where(query, [dr], dr.data == ^data)
  end

  defp filter_data(query, _), do: query

  defp filter_search(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [dr], ilike(dr.domain, ^q))
  end

  defp filter_search(query, _), do: query

  defp filter_select(query, _) do
    select(query, [dr], %{
      domain: dr.domain,
      name: dr.name,
      type: dr.type,
      data: dr.data,
      ttl: dr.ttl
    })
  end

  defp transform_list([]), do: []

  defp transform_list(x) do
    Enum.map(x, fn y -> transform(y) end)
  end

  defp transform(nil), do: nil
  defp transform(x), do: %{x | type: number_to_type(x.type)}

  def type_to_number("A"), do: 1
  def type_to_number("NS"), do: 2
  def type_to_number("CNAME"), do: 5
  def type_to_number("SOA"), do: 6
  def type_to_number("PTR"), do: 12
  def type_to_number("MX"), do: 15
  def type_to_number("TXT"), do: 16
  def type_to_number("AAAA"), do: 28
  def type_to_number("SRV"), do: 33
  def type_to_number("CAA"), do: 257
  def type_to_number(x) when is_integer(x), do: x
  def type_to_number(x), do: 1

  def number_to_type(1), do: "A"
  def number_to_type(2), do: "NS"
  def number_to_type(5), do: "CNAME"
  def number_to_type(6), do: "SOA"
  def number_to_type(12), do: "PTR"
  def number_to_type(15), do: "MX"
  def number_to_type(16), do: "TXT"
  def number_to_type(28), do: "AAAA"
  def number_to_type(33), do: "SRV"
  def number_to_type(257), do: "CAA"

  def type_to_number(:a), do: 1
  def type_to_number(:ns), do: 2
  def type_to_number(:cname), do: 5
  def type_to_number(:soa), do: 6
  def type_to_number(:wks), do: 11
  def type_to_number(:ptr), do: 12
  def type_to_number(:hinfo), do: 13
  def type_to_number(:mx), do: 15
  def type_to_number(:txt), do: 16
  def type_to_number(:aaaa), do: 28
  def type_to_number(:srv), do: 33
  def type_to_number(:ds), do: 43
  def type_to_number(:sshfp), do: 44
  def type_to_number(:rrsig), do: 46
  def type_to_number(:nsec), do: 47
  def type_to_number(:dnskey), do: 48
  def type_to_number(:https), do: 65
  def type_to_number(:spf), do: 99
  def type_to_number(:all), do: 255
  def type_to_number(:uri), do: 256
  def type_to_number(:caa), do: 257
  def type_to_number(x), do: x
end
