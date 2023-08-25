defmodule Ippan.Func.Validator do
  alias Phoenix.PubSub
  alias Ippan.Validator
  alias Ippan.Request.Source
  require SqliteStore
  require BalanceStore

  @type result :: Ippan.Request.result()
  @pubsub_server :cluster
  @token Application.compile_env(:ipncore, :token)
  @max_validators Application.compile_env(:ipncore, :max_validators)
  @topic "validator"
  @table_name "blockchain.validator"

  def new(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts, timestamp: timestamp},
        id,
        owner_id,
        hostname,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        stake,
        opts \\ %{}
      ) do
    cond do
      SqliteStore.exists?(conn, stmts, "exists_validator", id) ->
        :error

      @max_validators > SqliteStore.total(conn, stmts, "total_validator") ->
        :error

      true ->
        map_filter = Map.take(opts, Validator.optionals())
        pubkey = Fast64.decode64(pubkey)
        net_pubkey = Fast64.decode64(net_pubkey)

        case BalanceStore.subtract(dets, {account_id, @token}, stake) do
          :error ->
            :error

          _ ->
            validator =
              %Validator{
                id: id,
                hostname: hostname,
                name: name,
                pubkey: pubkey,
                net_pubkey: net_pubkey,
                owner: owner_id,
                fee: fee,
                fee_type: fee_type,
                stake: trunc(stake * 0.7),
                created_at: timestamp,
                updated_at: timestamp
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))

            SqliteStore.step(conn, stmts, "insert_doamin", Validator.to_list(validator))
            PubSub.broadcast(@pubsub_server, "validator", {"new", validator})
        end
    end
  end

  def update(
        %{id: account_id, conn: conn, dets: dets, timestamp: timestamp},
        id,
        opts
      ) do
    map_filter = Map.take(opts, Validator.editable())
    fee = EnvStore.network_fee()
    balance = {account_id, @token}

    case BalanceStore.subtract(dets, balance, fee) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, timestamp)

        SqliteStore.update(conn, @table_name, map, id: id)
        PubSub.broadcast(@pubsub_server, @topic, {"update", id, map})
    end
  end

  @spec delete(Source.t(), term) :: result()
  def delete(%{id: account_id, conn: conn, dets: dets, stmts: stmts}, id) do
    validator = SqliteStore.lookup_map(:validator, conn, stmts, "get_valdiator", id, Validator)

    if validator.stake > 0 do
      BalanceStore.income(dets, {account_id, @token}, validator.stake)
    end

    SqliteStore.step(conn, stmts, "delete_validator", [id])

    PubSub.broadcast(@pubsub_server, @topic, {"delete", id})
  end
end