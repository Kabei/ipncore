defmodule Ippan.Funx.Service do
  require BalanceStore
  alias Ippan.Utils
  require Sqlite

  def new(%{id: account_id, round: round_id}, id, name, extras, round_id) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    price = EnvStore.service_price()

    case BalanceStore.pay_burn(account_id, price) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)
        PayService.create(db_ref, id, name, extras, round_id)
    end
  end

  def update(
        %{id: account_id, size: size, validator: %{fa: fa, fb: fb, owner: vOwner}},
        id,
        map
      ) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)
        PayService.update(db_ref, map, id)
    end
  end

  def delete(_source, id) do
    db_ref = :persistent_term.get(:main_conn)
    PayService.delete(db_ref, id)
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
        max_amount
      ) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
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
          %{"max_amount" => max_amount},
          round_id
        )
    end
  end

  def unsubscribe(%{id: account_id}, service_id, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, account_id, token_id)
  end
end
