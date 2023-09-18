defmodule Ippan.Func.Dns do
  alias Ippan.{Domain, DNS}
  require SqliteStore
  require BalanceStore

  @token Application.compile_env(:ipncore, :token)
  @table_name "dns.dns"

  def new(
        %{
          id: account_id,
          conn: conn,
          dets: dets,
          stmts: stmts,
          validator: validator
        },
        fullname,
        type,
        data,
        ttl
      ) do
    fee = EnvStore.network_fee()
    balance_key = BalanceStore.gen_key(account_id, @token)
    balance_validator_key = BalanceStore.gen_key(validator.owner, @token)

    case BalanceStore.pay(dets, balance_key, balance_validator_key, fee) do
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

        SqliteStore.step(conn, stmts, "insert_dns", dns)
    end
  end

  def update(
        %{id: account_id, conn: conn, dets: dets, validator: validator},
        fullname,
        dns_hash16,
        params
      ) do
    dns_hash = Base.decode16(dns_hash16, case: :mixed)
    fee = EnvStore.network_fee()
    balance_key = BalanceStore.gen_key(account_id, @token)
    balance_validator_key = BalanceStore.gen_key(validator.owner, @token)

    case BalanceStore.pay(dets, balance_key, balance_validator_key, fee) do
      :error ->
        :error

      _ ->
        {_subdomain, domain} = Domain.split(fullname)

        ref =
          Map.take(params, DNS.editable())
          |> MapUtil.to_atoms()

        SqliteStore.update(conn, @table_name, ref, domain: domain, hash: dns_hash)
    end
  end

  def delete(%{conn: conn, stmts: stmts}, fullname) do
    {subdomain, domain} = Domain.split(fullname)

    case subdomain do
      "" ->
        SqliteStore.step(conn, stmts, "delete_dns", [domain])

      _ ->
        SqliteStore.step(conn, stmts, "delete_name_dns", [domain, subdomain])
    end
  end

  def delete(%{conn: conn, stmts: stmts}, fullname, type) when is_integer(type) do
    {subdomain, domain} = Domain.split(fullname)

    SqliteStore.step(conn, stmts, "delete_type_dns", [domain, subdomain, type])
  end

  def delete(%{conn: conn, stmts: stmts}, fullname, hash16) do
    {subdomain, domain} = Domain.split(fullname)
    hash = Base.decode16!(hash16, case: :mixed)
    SqliteStore.step(conn, stmts, "delete_hash_dns", [domain, subdomain, hash])
  end
end
