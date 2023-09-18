defmodule Ippan.Func.Domain do
  alias Ippan.Domain
  require SqliteStore
  require BalanceStore

  @table_name "assets.domain"

  @token Application.compile_env(:ipncore, :token)
  @one_day :timer.hours(24)

  def new(
        %{
          id: account_id,
          conn: conn,
          dets: dets,
          stmts: stmts,
          timestamp: timestamp
        },
        domain_name,
        owner,
        days,
        opts \\ %{}
      ) do
    price = Domain.price(domain_name, days)

    cond do
      SqliteStore.exists?(conn, stmts, "exists_domain", domain_name) ->
        :error

      true ->
        balance_key = BalanceStore.gen_key(account_id, @token)
        case BalanceStore.subtract(dets, balance_key, price) do
          :error ->
            :error

          _ ->
            map_filter = Map.take(opts, Domain.optionals())

            domain =
              %Domain{
                name: domain_name,
                owner: owner,
                created_at: timestamp,
                renewed_at: timestamp + days * @one_day,
                updated_at: timestamp
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))
              |> Domain.to_list()

            SqliteStore.step(conn, stmts, "insert_domain", domain)
        end
    end
  end

  def update(
        %{
          id: account_id,
          conn: conn,
          dets: dets,
          validator: validator,
          timestamp: timestamp
        },
        domain_name,
        opts \\ %{}
      ) do
    map_filter = Map.take(opts, Domain.editable())

    fee = EnvStore.network_fee()
    balance_key = BalanceStore.gen_key(account_id, @token)
    balance_validator = BalanceStore.gen_key(validator.owner, @token)

    case BalanceStore.pay(dets, balance_key, balance_validator, fee) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, timestamp)

        SqliteStore.update(conn, @table_name, map, name: domain_name)
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, domain_name) do
    case SqliteStore.step_change(conn, stmts, "delete_domain", [domain_name, account_id]) do
      1 ->
        SqliteStore.step(conn, stmts, "delete_dns", [domain_name])

      _ ->
        :error
    end
  end

  def renew(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts, timestamp: timestamp},
        name,
        days
      ) do
    price = Domain.price(name, days)

    case BalanceStore.subtract(dets, {account_id, @token}, price) do
      :error ->
        :error

      _ ->
        SqliteStore.step(conn, stmts, "renew_domain", [
          name,
          account_id,
          days * @one_day,
          timestamp
        ])
    end
  end
end
