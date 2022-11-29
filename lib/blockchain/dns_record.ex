defmodule Ipncore.DnsRecord do
  use Ecto.Schema
  import Ecto.Query
  alias Ipncore.{Domain, Repo}

  @base :dns
  @filename "dns.db"
  @dns_types ~w(a aaaa cname ptr mx txt srv caa ns)
  @max_domain_size 100
  @dns_max_ttl 2_147_483_647

  schema "dns_record" do
    field(:domain, :string)
    field(:type, :string)
    field(:value, :string)
    field(:ttl, :integer, default: 600)
  end

  def open do
    dir_path = Application.get_env(:ipncore, :data_path, "data")
    filename = Path.join([dir_path, @filename])
    DetsPlus.open_file(@base, file: filename, auto_save: 60_000)
  end

  def close do
    DetsPlus.close(@base)
  end

  def lookup(domain, type) do
    DetsPlus.lookup(@base, {domain, type})
    |> case do
      [{_, value, ttl}] ->
        {value, ttl}

      _ ->
        nil
    end
  end

  def check_put!(domain, type, from_address, value, ttl)
      when is_binary(domain) and is_binary(value) and ttl >= 0 and ttl <= @dns_max_ttl do
    if type not in @dns_types, do: throw("DNS record type not supported")
    if byte_size(domain) > @max_domain_size, do: throw("DNS record domain size exceeded")

    domain_key = Domain.extract_domain(domain)
    Domain.fetch!(domain_key, from_address)
  end

  def put!(_multi, domain, type, val, ttl, _channel) do
    type_atom = String.to_atom(type)

    value =
      case type_atom do
        :a ->
          Inet.to_ip(val)

        :aaaa ->
          Inet.to_ip(val)

        :txt ->
          [to_charlist(val)]

        _ ->
          to_charlist(val)
      end

    DetsPlus.insert(@base, {{domain, type_atom}, value, ttl})
  end

  def check_delete!(domain, type, from_address) do
    if type not in @dns_types, do: throw("Invalid DNS record type")
    if byte_size(domain) > @max_domain_size, do: throw("DNS record domain size exceeded")

    domain_key = Domain.extract_domain(domain)
    Domain.fetch!(domain_key, from_address)
    if DetsPlus.member?(@base, {domain, type}), do: throw("DNS record not exists")
  end

  def delete!(_multi, domain, type, _channel) do
    DetsPlus.delete(@base, {domain, type})
  end
end
