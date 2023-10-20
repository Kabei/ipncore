defmodule Ippan.Funx.Domain do
  alias Ippan.{Domain, DNS}
  require BalanceStore
  require Sqlite
  require Domain
  require DNS

  @token Application.compile_env(:ipncore, :token)
  @one_day 18_000

  def new(
        %{id: account_id, round: round_id},
        name,
        owner,
        days,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    price = Domain.price(name, days)

    cond do
      Domain.exists?(name) ->
        :error

      true ->
        dets = DetsPlux.get(:balance)
        tx = DetsPlux.tx(:balance)
        balance_key = DetsPlux.tuple(account_id, @token)

        case BalanceStore.subtract(dets, tx, balance_key, price) do
          :error ->
            :error

          _ ->
            map_filter = Map.take(opts, Domain.optionals())

            domain =
              %Domain{
                name: name,
                owner: owner,
                created_at: round_id,
                renewed_at: round_id + days * @one_day,
                updated_at: round_id
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))
              |> Domain.to_list()

            Domain.insert(domain)
        end
    end
  end

  def update(
        %{id: account_id, round: round_id, validator: validator},
        name,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    map_filter = Map.take(opts, Domain.editable())
    fee = EnvStore.network_fee()
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    balance_key = DetsPlux.tuple(account_id, @token)
    balance_validator = DetsPlux.tuple(validator.owner, @token)

    case BalanceStore.pay(dets, tx, balance_key, balance_validator, fee) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, round_id)

        Domain.update(map, name: name)
    end
  end

  def delete(%{id: account_id}, name) do
    db_ref = :persistent_term.get(:main_conn)

    case Domain.delete(name, account_id) do
      1 ->
        DNS.delete(name)

      _ ->
        :error
    end
  end

  def renew(
        %{id: account_id, round: round_id},
        name,
        days
      ) do
    db_ref = :persistent_term.get(:main_conn)
    price = Domain.price(name, days)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    key = DetsPlux.tuple(account_id, @token)

    case BalanceStore.subtract(dets, tx, key, price) do
      false -> :error
      _ -> Domain.renew(name, account_id, days * @one_day, round_id)
    end
  end
end
