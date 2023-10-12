defmodule Ippan.Func.Dns do
  alias Ippan.{Domain, DNS}
  require SqliteStore
  require BalanceStore

  @token Application.compile_env(:ipnworker, :token)
  @fullname_max_size 255
  @type_range [1, 2, 6, 15, 16, 28]
  @data_range 1..255
  @ttl_range 0..2_147_483_648

  def new(
        %{
          id: account_id,
          balance: {dets, tx},
          conn: conn,
          stmts: stmts,
          size: size
        },
        fullname,
        type,
        data,
        ttl
      )
      when byte_size(fullname) <= @fullname_max_size and
             type in @type_range and
             byte_size(data) in @data_range and
             ttl in @ttl_range do
    {_subdomain, domain} = Domain.split(fullname)

    dns_type = DNS.type_to_alpha(type)
    balance_key = DetsPlux.tuple(account_id, @token)

    cond do
      not Match.hostname?(fullname) ->
        raise IppanError, "Invalid hostname"

      not match?(
        {_, _, _, _, _value},
        :dnslib.resource(~c"#{fullname} IN #{ttl} #{dns_type} #{data}")
      ) ->
        raise IppanError, "DNS resource format error"

      not SqliteStore.exists?(conn, stmts, "exists_domain", [domain, account_id]) ->
        raise IppanError, "Invalid owner"

      true ->
        BalanceStore.requires!(dets, tx, balance_key, size)
    end
  end

  def update(
        %{id: account_id, conn: conn, balance: {dets, tx}, stmts: stmts},
        fullname,
        dns_hash16,
        params
      ) do
    map_filter = Map.take(params, DNS.editable())

    {_subdomain, domain} = Domain.split(fullname)

    dns_hash = Base.decode16!(dns_hash16, case: :mixed)

    fee = EnvStore.network_fee()

    dns =
      SqliteStore.lookup_map(:dns, conn, stmts, "get_dns", {domain, dns_hash}, DNS)

    balance_key = DetsPlux.tuple(account_id, @token)

    cond do
      map_filter == %{} ->
        raise IppanError, "Invalid optional arguments"

      map_filter != params ->
        raise IppanError, "Invalid optional arguments"

      not SqliteStore.exists?(conn, stmts, "owner_domain", [domain, account_id]) ->
        raise IppanError, "Invalid owner"

      not BalanceStore.has?(dets, tx, balance_key, fee) ->
        raise IppanError, "Insufficient balance"

      not match?(
        {_, _, _, _, _value},
        :dnslib.resource(~c"#{fullname} IN #{dns.ttl} #{dns.type} #{dns.data}")
      ) ->
        raise ArgumentError, "DNS resource format error"

      true ->
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_range(:ttl, @ttl_range)
        |> MapUtil.validate_bytes_range(:data, @data_range)

        :ok
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, fullname)
      when byte_size(fullname) <= @fullname_max_size do
    {_subdomain, domain} = Domain.split(fullname)

    if SqliteStore.exists?(conn, stmts, "owner_domain", [domain, account_id]) do
      :ok
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, fullname, type)
      when type in @type_range do
    {_subdomain, domain} = Domain.split(fullname)

    if SqliteStore.exists?(conn, stmts, "owner_domain", [domain, account_id]) do
      :ok
    else
      raise IppanError, "Invalid Owner"
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, fullname, hash16)
      when byte_size(hash16) == 32 do
    {_subdomain, domain} = Domain.split(fullname)

    cond do
      not Match.base16(hash16) ->
        raise IppanError, "Invalid hash"

      not SqliteStore.exists?(conn, stmts, "owner_domain", [domain, account_id]) ->
        raise IppanError, "Invalid Owner"

      true ->
        :ok
    end
  end
end
