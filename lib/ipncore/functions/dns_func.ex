defmodule Ippan.Func.Dns do
  alias Ippan.{Domain, DNS}
  @fullname_max_size 255
  @type_range [1, 2, 6, 15, 16, 28]
  @data_range 1..255
  @ttl_range 0..2_147_483_648

  # @dns_types ~w(A NS CNAME SOA PTR MX TXT AAAA SPF SRV DS SSHFP RRSIG NSEC DNSKEY CAA URI HINFO WKS)

  def new(
        %{id: account_id, size: size, validator: validator_id, timestamp: timestamp},
        fullname,
        type,
        data,
        ttl
      )
      when byte_size(fullname) <= @fullname_max_size and
             type in @type_range and
             byte_size(data) in @data_range and
             ttl in @ttl_range do
    {subdomain, domain} = Domain.split(fullname)

    dns_type = DNS.type_to_alpha(type)

    cond do
      not Match.domain?(domain) ->
        raise IppanError, "Invalid domain"

      not DomainStore.owner?(domain, account_id) ->
        raise IppanError, "Invalid owner"

      not match?(
        {_, _, _, _, _value},
        :dnslib.resource(~c"#{fullname} IN #{ttl} #{dns_type} #{data}")
      ) ->
        raise IppanError, "DNS resource error"

      true ->
        case ValidatorStore.lookup_map(validator_id) do
          nil ->
            raise IppanError, "Invalid validator"

          validator ->
            :ok = BalanceStore.send_fees(account_id, validator.owner, size, timestamp)

            %DNS{
              domain: domain,
              name: subdomain,
              data: data,
              type: type,
              ttl: ttl,
              hash: DNS.fun_hash([fullname, "#{type}", data])
            }
            |> DNS.to_list()
            |> DnsStore.insert_sync()
        end
    end
  end

  def update(
        %{id: account_id, validator: validator_id, timestamp: timestamp, size: size},
        fullname,
        dns_hash16,
        params
      ) do
    map_filter = Map.take(params, DNS.editable())
    {_subdomain, domain} = Domain.split(fullname)
    dns_hash = Base.decode16!(dns_hash16, case: :mixed)

    cond do
      map_filter != params ->
        raise IppanError, "Invalid optional arguments"

      not DomainStore.owner?(domain, account_id) ->
        raise IppanError, "Invalid owner"

      true ->
        case ValidatorStore.lookup_map(validator_id) do
          nil ->
            raise IppanError, "Invalid validator"

          validator ->
            dns_map = DnsStore.one([domain, dns_hash]) |> DNS.to_map()

            data = map_filter["data"]

            dns_type = DNS.type_to_alpha(dns_map.type)

            fullname = Domain.join(dns_map.name, dns_map.domain)

            if data do
              if not match?(
                   {_, _, _, _, _value},
                   :dnslib.resource(~c"#{fullname} IN #{dns_map.ttl} #{dns_type} #{data}")
                 ) do
                raise ArgumentError, "DNS resource error"
              end
            end

            ref =
              MapUtil.to_atoms(map_filter)
              |> MapUtil.validate_range(:ttl, @ttl_range)
              |> MapUtil.validate_bytes_range(:data, @data_range)

            :ok = BalanceStore.send_fees(account_id, validator.owner, size, timestamp)

            DnsStore.update(ref, domain: domain, hash: dns_hash)
        end
    end
  end

  def delete(%{id: account_id}, fullname) when byte_size(fullname) <= @fullname_max_size do
    {subdomain, domain} = Domain.split(fullname)

    if DomainStore.owner?(domain, account_id) do
      case subdomain do
        "" ->
          DnsStore.delete([domain])

        subdomain ->
          DnsStore.step_change("delete_name", [domain, subdomain])
      end
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{id: account_id}, fullname, type) when type in @type_range do
    {subdomain, domain} = Domain.split(fullname)

    if DomainStore.owner?(domain, account_id) do
      DnsStore.step_change("delete_type", [domain, subdomain, type])
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{id: account_id}, fullname, hash16) do
    {subdomain, domain} = Domain.split(fullname)
    hash = Base.decode16!(hash16, case: :mixed)

    if DomainStore.owner?(domain, account_id) do
      DnsStore.step_change("delete_hash", [domain, subdomain, hash])
    else
      raise IppanError, "Invalid Owner"
    end
  end
end
