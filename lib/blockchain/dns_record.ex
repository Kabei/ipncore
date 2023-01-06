defmodule Ipncore.DnsRecord do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Domain, Repo, Tx}
  alias __MODULE__

  # @table :dns
  @base :dns
  @filename "dns.db"
  @dns_types ~w(a aaaa cname ptr mx txt srv caa ns soa)
  @dns_types_multi_records ~w(a aaaa)a
  @max_domain_size 50
  @min_ttl 300
  @max_ttl 2_147_483_647
  @max_records 50
  @max_dns_record_items 3
  @max_bytes_value 255
  @price 500

  schema "dns_record" do
    field(:domain, :string)
    field(:type, :string)
    field(:value, :string)
    field(:ttl, :integer, default: 3600)
    field(:root, :string)
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

  def lookup(domain, type) do
    DetsPlus.lookup(@base, {domain, type})
    |> case do
      [{_, value, ttl, _}] ->
        {value, ttl}

      _ ->
        nil
    end
  end

  def check_push!(domain_name, type, from_address, value, ttl)
      when is_binary(domain_name) and is_binary(value) and ttl >= @min_ttl and ttl <= @max_ttl do
    if type not in @dns_types, do: throw("DNS record type not supported")
    if byte_size(domain_name) > @max_domain_size, do: throw("DNS record domain size exceeded")
    if byte_size(value) > @max_bytes_value, do: throw("DNS record value size exceeded")

    domain_root = Domain.extract_root(domain_name)
    domain = Domain.fetch!(domain_root, from_address)
    if @max_records < domain.records, do: throw("Max record exceeded")

    Tx.check_fee!(from_address, @price)

    :ok
  end

  def event_push!(
        multi,
        event_id,
        from_address,
        domain_name,
        type,
        val,
        ttl,
        validator_host,
        timestamp,
        replace,
        channel
      ) do
    type_atom = String.to_atom(type)
    domain_root = Domain.extract_root(domain_name)
    domain = Domain.fetch!(domain_root)
    query_name = to_charlist(domain_name)

    value =
      case type_atom do
        :a ->
          Inet.to_ip(val)

        :aaaa ->
          Inet.to_ip(val)

        :txt ->
          to_charlist(val)

        _ ->
          to_charlist(val)
      end

    {object, count} =
      cond do
        # allow push multiple records
        replace == false and type_atom in @dns_types_multi_records ->
          case lookup(query_name, type_atom) do
            {values, _} when is_list(values) ->
              n = length(values)
              if n >= @max_dns_record_items, do: throw("Invalid max values in dns record")
              {{{query_name, type_atom}, values ++ [value], ttl, domain_root}, n + 1}

            {result, _} ->
              {{{query_name, type_atom}, [result, value], ttl, domain_root}, 2}

            _ ->
              {{{query_name, type_atom}, value, ttl, domain_root}, 1}
          end

        # single records
        true ->
          {{{query_name, type_atom}, value, ttl, domain_root}, 1}
      end

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

    put(object)

    multi = Domain.count_records(multi, domain, channel, count)

    struct = %{
      domain: domain_name,
      type: type,
      value: val,
      ttl: ttl,
      root: domain_root
    }

    # Add multi upsert
    Ecto.Multi.insert_all(multi, :dns, DnsRecord, [struct], prefix: channel, returning: false)
  end

  def check_drop!([domain_name], from_address) do
    if byte_size(domain_name) > @max_domain_size, do: throw("DNS record domain size exceeded")

    domain_root = Domain.extract_root(domain_name)
    Domain.fetch!(domain_root, from_address)
  end

  def check_drop!([domain_name, type], from_address) do
    if type not in @dns_types, do: throw("Invalid DNS record type")
    if byte_size(domain_name) > @max_domain_size, do: throw("DNS record domain size exceeded")

    domain_root = Domain.extract_root(domain_name)
    Domain.fetch!(domain_root, from_address)
    if DetsPlus.member?(@base, {domain_name, type}), do: throw("DNS record not exists")
  end

  def event_drop!(multi, [domain_name], channel) do
    domain_root = Domain.extract_root(domain_name)
    domain = Domain.fetch!(domain_root)

    n =
      DetsPlus.reduce(@base, 0, fn {key, val, _ttl, _root}, acc ->
        cond do
          key == domain_name ->
            DetsPlus.delete(@base, key)
            acc + if is_list(val), do: length(val), else: 1

          true ->
            acc
        end
      end)

    multi = Domain.uncount_records(multi, domain, channel, n)

    queryable = from(dr in DnsRecord, where: dr.domain == ^domain_name)
    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel, returning: false)
  end

  def event_drop!(multi, [domain_name, type], channel) do
    domain_root = Domain.extract_root(domain_name)
    domain = Domain.fetch!(domain_root)

    {val, _ttl} = lookup(domain_name, type)
    DetsPlus.delete(@base, {domain_name, type})
    n = if is_list(val), do: length(val), else: 1
    multi = Domain.uncount_records(multi, domain, channel, n)

    queryable = from(dr in DnsRecord, where: dr.domain == ^domain_name and dr.type == ^type)
    Ecto.Multi.delete_all(multi, :delete, queryable, prefix: channel, returning: false)
  end

  def delete_by_root(multi, domain_root, channel) do
    DetsPlus.reduce(@base, 0, fn {key, _value, _ttl, root}, acc ->
      cond do
        root == domain_root ->
          DetsPlus.delete(@base, key)
          acc + 1

        true ->
          acc
      end
    end)

    queryable = from(dr in DnsRecord, where: dr.root == ^domain_root)
    Ecto.Multi.delete_all(multi, :delete_all, queryable, prefix: channel)
  end

  def one(domain, type, channel) do
    from(dr in DnsRecord, where: dr.domain == ^domain and dr.type == ^type)
    |> filter_select(nil)
    |> Repo.one(prefix: channel)
  end

  def all(params) do
    from(dr in DnsRecord)
    |> filter_domain(params)
    |> filter_type(params)
    |> filter_root(params)
    |> filter_value(params)
    |> filter_search(params)
    |> filter_select(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
  end

  defp filter_domain(query, %{"domain" => domain}) do
    where(query, [dr], dr.domain == ^domain)
  end

  defp filter_domain(query, _), do: query

  defp filter_type(query, %{"type" => type}) do
    where(query, [dr], dr.type == ^type)
  end

  defp filter_type(query, _), do: query

  defp filter_root(query, %{"root" => root}) do
    where(query, [dr], dr.root == ^root)
  end

  defp filter_root(query, _), do: query

  defp filter_value(query, %{"value" => value}) do
    where(query, [dr], dr.value == ^value)
  end

  defp filter_value(query, _), do: query

  defp filter_search(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [dr], ilike(dr.domain, ^q))
  end

  defp filter_search(query, _), do: query

  defp filter_select(query, _) do
    select(query, [dr], %{
      domain: dr.domain,
      type: dr.type,
      value: dr.value,
      ttl: dr.ttl
    })
  end
end
