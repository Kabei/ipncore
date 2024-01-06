defmodule Ippan.Funx.Validator do
  alias Ippan.{Utils, Validator}
  alias Phoenix.PubSub
  require Validator
  require Sqlite
  require BalanceStore
  require Logger

  @app Mix.Project.config()[:app]
  @pubsub :pubsub
  @max_validators Application.compile_env(@app, :max_validators)
  @topic "validator"

  def join(
        %{id: account_id, round: round_id},
        hostname,
        port,
        owner_id,
        name,
        pubkey,
        net_pubkey,
        fa \\ 0,
        fb \\ 1,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    total = Validator.total()

    cond do
      Validator.exists_host?(hostname) ->
        :error

      @max_validators <= total ->
        :error

      true ->
        map_filter = Map.take(opts, Validator.optionals())
        pubkey = Fast64.decode64(pubkey)
        net_pubkey = Fast64.decode64(net_pubkey)
        dets = DetsPlux.get(:balance)
        tx = DetsPlux.tx(:balance)
        price = Validator.calc_price(total)

        case BalanceStore.pay_burn(account_id, price) do
          :error ->
            :error

          _ ->
            next_id = Validator.next_id()

            validator =
              %Validator{
                id: next_id,
                hostname: hostname,
                port: port,
                name: name,
                pubkey: pubkey,
                net_pubkey: net_pubkey,
                owner: owner_id,
                fa: fa,
                fb: fb,
                created_at: round_id,
                updated_at: round_id
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))

            Validator.insert(Validator.to_list(validator))

            event = %{"event" => "validator.new", "data" => Validator.to_text(validator)}
            PubSub.broadcast(@pubsub, @topic, event)
        end
    end
  end

  def update(
        %{id: account_id, size: size, validator: %{fa: fa, fb: fb}, round: round_id},
        id,
        opts
      ) do
    map_filter = Map.take(opts, Validator.editable())
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_burn(account_id, fees) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, round_id)

        db_ref = :persistent_term.get(:main_conn)
        Validator.update(map, id)

        # transform to text
        fun = fn x -> Base.decode64!(x) end

        map =
          map
          |> MapUtil.transform(:pubkey, fun)
          |> MapUtil.transform(:net_pubkey, fun)

        event = %{"event" => "validator.update", "data" => %{"id" => id, "args" => map}}
        PubSub.broadcast(@pubsub, @topic, event)
    end
  end

  def active(
        %{
          id: account_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          round: round_id
        },
        id,
        active
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        Validator.put_active(id, active, round_id)
        event = %{"event" => "validator.active", "data" => %{"id" => id, "active" => active}}
        PubSub.broadcast(@pubsub, @topic, event)
    end
  end

  def leave(_source, id) do
    db_ref = :persistent_term.get(:main_conn)
    Validator.delete(id)

    event = %{"event" => "validator.leave", "data" => id}
    PubSub.broadcast(@pubsub, @topic, event)
  end

  def env_put(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        name,
        value
      ) do
    db_ref = :persistent_term.get(:main_conn)
    validator = Validator.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case is_nil(validator) do
      true ->
        :error

      _false ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = Map.put(validator.env, name, value)
            map = %{env: CBOR.encode(result), updated_at: round_id}
            Validator.update(map, id)
        end
    end
  end

  def env_delete(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        name
      ) do
    db_ref = :persistent_term.get(:main_conn)
    validator = Validator.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case is_nil(validator) do
      true ->
        :error

      _false ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = Map.delete(validator.env, name)
            map = %{env: CBOR.encode(result), updated_at: round_id}
            Validator.update(map, id)
        end
    end
  end
end
