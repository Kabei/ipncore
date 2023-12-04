defmodule Ippan.Funx.Validator do
  alias Ippan.{Utils, Validator}
  alias Phoenix.PubSub
  require Validator
  require Sqlite
  require BalanceStore

  @app Mix.Project.config()[:app]
  @pubsub :pubsub
  @max_validators Application.compile_env(@app, :max_validators)
  @topic "validator"

  def new(
        %{id: account_id, round: round_id},
        owner_id,
        hostname,
        port,
        name,
        pubkey,
        net_pubkey,
        fa \\ 0,
        fb \\ 1,
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
        map_filter = Map.take(opts, Validator.optionals())
        pubkey = Fast64.decode64(pubkey)
        net_pubkey = Fast64.decode64(net_pubkey)
        dets = DetsPlux.get(:balance)
        tx = DetsPlux.tx(:balance)
        price = Validator.calc_price(next_id)

        case BalanceStore.pay_burn(account_id, price) do
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
                fa: fa,
                fb: fb,
                created_at: round_id,
                updated_at: round_id
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))

            Validator.insert(Validator.to_list(validator))

            if next_id == :persistent_term.get(:vid) do
              Validator.self(validator)
            end

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

        if id == :persistent_term.get(:vid) do
          v = Ippan.Validator.get(id)
          Ippan.Validator.self(v)
        end

        # transform to text
        fun = fn x -> Utils.encode64(x) end

        map =
          map
          |> MapUtil.transform(:pubkey, fun)
          |> MapUtil.transform(:net_pubkey, fun)

        event = %{"event" => "validator.update", "data" => %{"id" => id, "args" => map}}
        PubSub.broadcast(@pubsub, @topic, event)
    end
  end

  def delete(_source, id) do
    db_ref = :persistent_term.get(:main_conn)
    Validator.delete(id)

    event = %{"event" => "validator.delete", "data" => id}
    PubSub.broadcast(@pubsub, @topic, event)
  end
end
