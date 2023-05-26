defmodule Ippan.Func.Dns do
  alias Ippan.{Domain, DNS}
  @fullname_max_size 255
  @type_range 0..255
  @data_range 1..255
  @ttl_range 0..2_147_483_648
  @token Default.token()

  @dns_types ~w(A NS CNAME SOA PTR MX TXT AAAA SPF SRV DS SSHFP RRSIG NSEC DNSKEY CAA URI HINFO WKS)

  def new(%{account: account, timestamp: timestamp}, fullname, type, data, ttl)
      when byte_size(fullname) <= @fullname_max_size and
             type in @type_range and
             byte_size(data) in @data_range and
             ttl in @ttl_range do
    {subdomain, domain} = Domain.split(fullname)

    cond do
      not Match.domain?(domain) ->
        raise IppanError, "Invalid domain"

      type not in @dns_types ->
        raise IppanError, "DNS record type not supported"

      not match?({_, _, _, _, _value}, :dnslib.resource('#{domain} IN #{ttl} #{type} #{data}')) ->
        raise IppanError, "DNS resource error"

      true ->
        validator = ValidatorStore.lookup(account.validator)
        :ok = BalanceStore.send(account.id, validator.owner, @token, 500, timestamp)

        %DNS{
          domain: domain,
          name: subdomain,
          data: data,
          ttl: ttl
        }
        |> DNS.to_list()
        |> DnsStore.insert()
    end
  end

  def update(%{account: account, timestamp: timestamp}, fullname, hash, opts \\ %{})
      when byte_size(fullname) <= @fullname_max_size do
    map_filter = Map.take(opts, DNS.editable())

    {_subdomain, domain} = Domain.split(fullname)

    cond do
      opts == %{} or map_filter != opts ->
        raise IppanError, "Invalid option field"

      not Match.domain?(domain) ->
        raise IppanError, "Invalid domain"

      true ->
        if DomainStore.owner?(domain, account.id) do
          dns_map =
            DnsStore.lookup([fullname, hash])
            |> DNS.to_map()

          {:ok, validator} = ValidatorStore.lookup(account.validator)
          :ok = BalanceStore.send_fees(account.id, validator.owner, 500, timestamp)

          data = map_filter[:data]

          if data do
            if not match?(
                 {_, _, _, _, _value},
                 :dnslib.resource('#{domain} IN #{dns_map.ttl} #{dns_map.type} #{data}')
               ) do
              raise IppanError, "DNS resource error"
            end
          end

          dns_map
          |> Map.merge(map_filter)
          |> MapUtil.validate_range(:ttl, @ttl_range)
          |> MapUtil.validate_bytes_range(:data, @data_range)
          |> DnsStore.update(domain: domain, hash: hash)
        end
    end
  end

  def delete(%{account: account}, fullname) when byte_size(fullname) <= @fullname_max_size do
    {subdomain, domain} = Domain.split(fullname)

    if DomainStore.owner?(domain, account.id) do
      case subdomain do
        "" ->
          DnsStore.delete(domain)

        subdomain ->
          DnsStore.execute_fetch("delete_name", [domain, subdomain])
      end
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{account: account}, fullname, type) when type in @type_range do
    {subdomain, domain} = Domain.split(fullname)

    if DomainStore.owner?(domain, account.id) do
      DnsStore.execute_fetch("delete_type", [domain, subdomain, type])
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{account: account}, fullname, hash) do
    {subdomain, domain} = Domain.split(fullname)

    if DomainStore.owner?(domain, account.id) do
      DnsStore.execute_fetch("delete_hash", [domain, subdomain, hash])
    else
      raise IppanError, "Invalid Owner"
    end
  end
end
