defmodule Ippan.Funx.Validator do
  alias Ippan.{ClusterNodes, Validator}
  alias Phoenix.PubSub
  require Validator
  require Sqlite
  require BalanceStore

  @pubsub :pubsub
  @token Application.compile_env(:ipncore, :token)
  @max_validators Application.compile_env(:ipncore, :max_validators)
  @topic "validator"

  def new(
        %{id: account_id, round: round_id},
        owner_id,
        hostname,
        port,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    next_id = Validator.next_id()

    cond do
      Validator.exists_host?(hostname) ->
        :error

      @max_validators <= next_id ->
        :error

      true ->
        stake = Validator.calc_price(next_id)
        map_filter = Map.take(opts, Validator.optionals())
        pubkey = Fast64.decode64(pubkey)
        net_pubkey = Fast64.decode64(net_pubkey)
        dets = DetsPlux.get(:balance)
        tx = DetsPlux.tx(:balance)
        balance_key = DetsPlux.tuple(account_id, @token)

        case BalanceStore.subtract(dets, tx, balance_key, stake) do
          :error ->
            :error

          _ ->
            validator =
              %Validator{
                id: next_id,
                hostname: hostname,
                port: port,
                name: name,
                pubkey: pubkey,
                net_pubkey: net_pubkey,
                owner: owner_id,
                fee: fee,
                fee_type: fee_type,
                stake: trunc(stake * 0.7),
                created_at: round_id,
                updated_at: round_id
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))

            Validator.insert(Validator.to_list(validator))

            if next_id == :persistent_term.get(:validator) do
              :persistent_term.put(:validator, validator)
            end

            event = %{"event" => "validator.new", "data" => validator}
            PubSub.broadcast(@pubsub, @topic, event)
            ClusterNodes.broadcast(event)
        end
    end
  end

  def update(
        %{id: account_id, round: round_id},
        id,
        opts
      ) do
    map_filter = Map.take(opts, Validator.editable())
    fee = EnvStore.network_fee()
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    balance_key = DetsPlux.tuple(account_id, @token)

    case BalanceStore.subtract(dets, tx, balance_key, fee) do
      false ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, round_id)

        Validator.update(map, id: id)

        event = %{"event" => "validator.update", "data" => %{"id" => id, "args" => map}}
        PubSub.broadcast(@pubsub, @topic, event)
        ClusterNodes.broadcast(event)
    end
  end

  def delete(%{id: account_id}, id) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    validator = Validator.get(id)

    if validator.stake > 0 do
      balance_key = DetsPlux.tuple(account_id, @token)
      BalanceStore.income(dets, tx, balance_key, validator.stake)
    end

    Validator.delete(id)

    event = %{"event" => "validator.delete", "data" => id}
    PubSub.broadcast(@pubsub, @topic, event)
    ClusterNodes.broadcast(event)
  end
end
