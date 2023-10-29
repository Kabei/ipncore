defmodule Ippan.Funx.Dns do
  alias Ippan.{Domain, DNS, Utils}
  require BalanceStore
  require Sqlite
  require DNS

  def new(
        %{id: account_id, size: size, validator: %{fa: fa, fb: fb, owner: vOwner}},
        fullname,
        type,
        data,
        ttl
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        {subdomain, domain} = Domain.split(fullname)

        dns =
          %DNS{
            domain: domain,
            name: subdomain,
            data: data,
            type: type,
            ttl: ttl,
            hash: DNS.fun_hash([fullname, "#{type}", data])
          }
          |> DNS.to_list()

        DNS.insert(dns)
    end
  end

  def update(
        %{id: account_id, size: size, validator: %{fa: fa, fb: fb, owner: vOwner}},
        fullname,
        dns_hash16,
        params
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        dns_hash = Base.decode16(dns_hash16, case: :mixed)
        {_subdomain, domain} = Domain.split(fullname)

        ref =
          Map.take(params, DNS.editable())
          |> MapUtil.to_atoms()

        DNS.update(ref, domain, dns_hash)
    end
  end

  def delete(_, fullname) do
    db_ref = :persistent_term.get(:main_conn)
    {subdomain, domain} = Domain.split(fullname)

    case subdomain do
      "" ->
        DNS.delete(domain)

      _ ->
        DNS.delete(domain, subdomain)
    end
  end

  def delete(_, fullname, type) when is_integer(type) do
    {subdomain, domain} = Domain.split(fullname)
    db_ref = :persistent_term.get(:main_conn)
    DNS.delete_type(domain, subdomain, type)
  end

  def delete(_, fullname, hash16) do
    {subdomain, domain} = Domain.split(fullname)
    hash = Base.decode16!(hash16, case: :mixed)
    db_ref = :persistent_term.get(:main_conn)
    DNS.delete_hash(domain, subdomain, hash)
  end
end
