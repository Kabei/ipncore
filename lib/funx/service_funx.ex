defmodule Ippan.Funx.Service do
  require BalanceStore
  alias Ippan.{Token, Utils}
  require Ippan.Token
  require Sqlite

  @app Mix.Project.config()[:app]
  @max_services Application.compile_env(@app, :max_services, 0)
  @token Application.compile_env(@app, :token)
  @skey "services"

  def new(%{id: account_id, round: round_id}, id, name, owner, image, extra) do
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    price = EnvStore.service_price()
    stats = Stats.new()

    cond do
      @max_services != 0 and @max_services <= Stats.get(stats, @skey) ->
        :error

      BalanceStore.pay_burn(account_id, price) == :error ->
        :error

      true ->
        db_ref = :persistent_term.get(:main_conn)
        PayService.create(db_ref, id, name, owner, image, extra, round_id)
        Stats.incr(stats, @skey, 1)
    end
  end

  def update(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        map
      ) do
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    fees = Utils.calc_fees(fa, fb, size)
    db_ref = :persistent_term.get(:main_conn)

    case PayService.get(db_ref, id) do
      nil ->
        :error

      %{name: current_name, owner: current_owner, image: current_image, extra: current_extra} ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            name = Map.get(map, "name", current_name)
            image = Map.get(map, "image", current_image)
            owner = Map.get(map, "owner", current_owner)

            extra =
              Map.drop(map, ["name", "image", "owner"])

            PayService.update(
              db_ref,
              %{
                "name" => name,
                "image" => image,
                "owner" => owner,
                "extra" => CBOR.encode(Map.merge(current_extra, extra)),
                "updated_at" => round_id
              },
              id
            )
        end
    end
  end

  def delete(_source, id) do
    db_ref = :persistent_term.get(:main_conn)

    cond do
      not PayService.exists?(db_ref, id) ->
        :error

      true ->
        PayService.delete(db_ref, id)
        stats = Stats.new()
        Stats.incr(stats, @skey, -1)
    end
  end

  def withdraw(
        %{id: account_id, size: size, validator: %{fa: fa, fb: fb, owner: vOwner}},
        service_id,
        token_id,
        amount
      ) do
    db_ref = :persistent_term.get(:main_conn)
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    tfees = Utils.calc_fees(fa, fb, size)
    %{env: env} = Token.get(token_id)
    tax = round(amount * Map.get(env, "service.tax", 0))

    BalanceStore.pay service_id, token_id, amount, tfees do
      total = amount - tax

      if total > 0 do
        BalanceStore.send(account_id, token_id, total)
      end

      reserve = Utils.calc_reserve(tfees)
      fees = tfees - reserve

      validator_balance = BalanceStore.load(vOwner, @token)
      BalanceStore.fees(validator_balance, fees)
      BalanceStore.reserve(reserve)

      BalanceStore.burn(service_id, token_id, tax)
    end
  end

  def subscribe(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        service_id,
        token_id,
        extra
      ) do
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)

        SubPay.subscribe(
          db_ref,
          service_id,
          account_id,
          token_id,
          extra,
          round_id
        )
    end
  end

  def unsubscribe(%{id: account_id}, service_id) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, account_id)
  end

  def unsubscribe(%{id: account_id}, service_id, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, account_id, token_id)
  end

  def kick(_source, service_id, payer) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, payer)
  end

  def kick(_source, service_id, payer, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, payer, token_id)
  end
end
