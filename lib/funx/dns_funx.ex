defmodule Ippan.Funx.Dns do
  alias Ippan.{Domain, DNS}
  require BalanceStore
  require Sqlite
  require DNS

  @token Application.compile_env(:ipncore, :token)

  def new(
        %{id: account_id, validator: validator},
        fullname,
        type,
        data,
        ttl
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    fee = EnvStore.network_fee()
    key = DetsPlux.tuple(account_id, @token)
    to_key = DetsPlux.tuple(validator.owner, @token)

    case BalanceStore.pay(dets, tx, key, to_key, fee) do
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
        %{id: account_id, validator: validator},
        fullname,
        dns_hash16,
        params
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    dns_hash = Base.decode16(dns_hash16, case: :mixed)
    fee = EnvStore.network_fee()
    balance_key = DetsPlux.tuple(account_id, @token)
    balance_validator_key = DetsPlux.tuple(validator.owner, @token)

    case BalanceStore.pay(dets, tx, balance_key, balance_validator_key, fee) do
      :error ->
        :error

      _ ->
        {_subdomain, domain} = Domain.split(fullname)

        ref =
          Map.take(params, DNS.editable())
          |> MapUtil.to_atoms()

        DNS.update(ref, domain: domain, hash: dns_hash)
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
