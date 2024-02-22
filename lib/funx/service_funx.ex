defmodule Ippan.Funx.Service do
  require BalanceStore
  alias Ippan.Utils
  require Sqlite

  def new(%{id: account_id, round: round_id}, id, name, extra) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    price = EnvStore.service_price()

    case BalanceStore.pay_burn(account_id, price) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)
        PayService.create(db_ref, id, name, extra, round_id)
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
    db_ref = :persistent_term.get(:main_conn)

    case PayService.get(db_ref, id) do
      nil ->
        :error

      service ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            extra = Map.delete(map, "name") |> Map.merge(service.extra)
            name = Map.get(map, "name", service.name)
            PayService.update(db_ref, %{"name" => name, "extra" => CBOR.encode(extra)}, id)
        end
    end
  end

  def delete(_source, id) do
    db_ref = :persistent_term.get(:main_conn)

    cond do
      not PayService.exists?(db_ref, id) ->
        raise IppanError, "Not exists service: #{id}"

      true ->
        PayService.delete(db_ref, id)
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
