defmodule Ippan.Func.Validator do
  require Global
  import Guards
  alias Phoenix.PubSub
  alias Ippan.Validator
  alias Ippan.Request.Source

  @type result :: Ippan.Request.result()
  @pubsub_server :verifiers
  @token Application.compile_env(:ipncore, :token)
  @max_validators Application.compile_env(:ipncore, :max_validators)

  def pre_new(
        %{id: account_id, hash: hash, round: round, timestamp: timestamp},
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
      )
      when is_positive(id) and
             byte_size(name) <= 20 and between_size(hostname, 4, 50) and fee_type in 0..2 and
             fee > 0 and is_float(fee) do
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)
    net_pubkey = Fast64.decode64(net_pubkey)

    cond do
      fee_type == 0 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      fee_type == 2 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      byte_size(net_pubkey) > 897 ->
        raise IppanError, "Invalid net_pubkey size #{byte_size(net_pubkey)}"

      byte_size(pubkey) > 897 ->
        raise IppanError, "Invalid pubkey size"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      not Match.hostname?(hostname) ->
        raise IppanError, "Invalid hostname"

      stake != EnvStore.validator_stake() ->
        raise IppanError, "Invalid stake amount"

      @max_validators < ValidatorStore.total() ->
        raise IppanError, "Maximum tokens exceeded"

      true ->
        %Validator{
          id: id,
          hostname: hostname,
          name: name,
          pubkey: pubkey,
          owner: owner_id,
          fee: fee,
          fee_type: fee_type,
          stake: 0,
          created_at: timestamp,
          updated_at: timestamp
        }
        |> Map.merge(MapUtil.to_atoms(map_filter))
        |> MapUtil.validate_url(:avatar)

        case BalanceStore.balance(
               account_id,
               @token,
               stake
             ) do
          :ok ->
            MessageStore.approve_df(round, timestamp, hash)

          _ ->
            raise IppanError, "Insufficient balance"
        end
    end
  end

  def new(
        %{id: account_id, timestamp: timestamp},
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
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)
    net_pubkey = Fast64.decode64(net_pubkey)

    1 = BalanceStore.burn(account_id, @token, stake, timestamp)

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
        stake: stake,
        created_at: timestamp,
        updated_at: timestamp
      }
      |> Map.merge(MapUtil.to_atoms(map_filter))
      |> MapUtil.validate_url(:avatar)

    validator
    |> Validator.to_list()
    |> ValidatorStore.insert_sync()

    PubSub.broadcast(@pubsub_server, "validator", {"new", validator})
  end

  def pre_update(
        %{id: account_id, hash: hash, round: round, timestamp: timestamp},
        id,
        opts \\ %{}
      ) do
    map_filter = Map.take(opts, Validator.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not ValidatorStore.owner?(id, account_id) ->
        raise IppanError, "Invalid owner"

      true ->
        map =
          MapUtil.to_atoms(map_filter)
          |> MapUtil.validate_hostname(:hostname)
          |> MapUtil.validate_length_range(:name, 1..20)
          |> MapUtil.validate_url(:url)
          |> MapUtil.validate_value(:fee, :gt, 0)
          |> MapUtil.validate_range(:fee_type, 0..2)
          |> MapUtil.transform(
            :pubkey,
            fn x ->
              case Fast64.decode64(x) do
                j when byte_size(j) > 897 ->
                  raise IppanError, "Invalid pubkey"

                j ->
                  j
              end
            end
          )
          |> MapUtil.transform(
            :net_pubkey,
            fn x ->
              case Fast64.decode64(x) do
                j when byte_size(j) > 897 ->
                  raise IppanError, "Invalid net_pubkey"

                j ->
                  j
              end
            end
          )
          |> Map.put(:updated_at, timestamp)

        MessageStore.update(%{round: round, args: :erlang.term_to_binary(map)},
          hash: hash,
          timestamp: timestamp
        )

        PubSub.broadcast(@pubsub_server, "validator", {"update", id, opts})
    end
  end

  def update(_, id, map) do
    ValidatorStore.update(map, id: id)

    PubSub.broadcast(@pubsub_server, "validator", {"update", id, map})
  end

  def pre_delete(%{id: account_id, hash: hash, round: round, timestamp: timestamp}, id) do
    validator = ValidatorStore.lookup([id])

    cond do
      not Global.owner?(account_id) or validator.owner != account_id ->
        raise IppanError, "Invalid owner"

      true ->
        MessageStore.approve_df(round, timestamp, hash)
    end
  end

  @spec delete(Source.t(), term) :: result()
  def delete(%{id: account_id, timestamp: timestamp}, id) do
    validator = ValidatorStore.lookup([id])

    if validator.stake > 0 do
      BalanceStore.income(account_id, @token, validator.stake, timestamp)
    end

    ValidatorStore.delete([id])

    PubSub.broadcast(@pubsub_server, "validator", {"delete", id})
  end
end
