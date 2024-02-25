defmodule Ippan.Funx.Service do
  require BalanceStore
  alias Ippan.Utils
  require Sqlite

  def new(%{id: account_id, round: round_id}, id, name, image, extra) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    price = EnvStore.service_price()

    case BalanceStore.pay_burn(account_id, price) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)
        PayService.create(db_ref, id, name, image, extra, round_id)
    end
  end

  def update(
        %{id: account_id, round: round_id, size: size, validator: %{fa: fa, fb: fb, owner: vOwner}},
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

          %{name: current_name, extra: current_extra} ->
            case BalanceStore.pay_fee(account_id, vOwner, fees) do
              :error ->
                :error

              _ ->
                name = Map.get(map, "name", current_name)
                extra = Map.delete(map, "name") |> Map.merge(current_extra)
                PayService.update(db_ref, %{
                  "name" => name,
                  "extra" => CBOR.encode(extra),
                  "updated_at" => round_id}, id)
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
          extra,
          round_id
        )
    end
  end

  def unsubscribe(%{id: account_id}, service_id, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    SubPay.unsubscribe(db_ref, service_id, account_id, token_id)
  end
end
