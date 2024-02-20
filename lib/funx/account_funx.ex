defmodule Ippan.Funx.Wallet do
  require BalanceStore
  alias Ippan.{Utils}

  def new(%{id: account_id}, pubkey, sig_type, validator_id, fa, fb) do
    pubkey = Fast64.decode64(pubkey)
    tx = DetsPlux.tx(:wallet)

    DetsPlux.put(
      tx,
      {account_id, pubkey, sig_type, %{"fa" => fa, "fb" => fb, "vid" => validator_id}}
    )
  end

  def subscribe(
        %{
          id: from,
          validator: %{owner: vOwner},
          size: size
        },
        validator_id,
        fa,
        fb
      ) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(from, vOwner, fees) do
      :error ->
        :error

      _ ->
        wdets = DetsPlux.get(:wallet)
        wtx = DetsPlux.tx(:wallet)
        DetsPlux.get_cache(wdets, wtx, from)
        DetsPlux.update_element(wtx, from, 4, %{"fa" => fa, "fb" => fb, "vid" => validator_id})
    end
  end

  def edit_key(
        %{
          id: from,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          size: size
        },
        pubkey,
        sig_type
      ) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    fees = Utils.calc_fees(fa, fb, size)
    pubkey = Fast64.decode64(pubkey)

    case BalanceStore.pay_fee(from, vOwner, fees) do
      :error ->
        :error

      _ ->
        wtx = DetsPlux.tx(:wallet)
        DetsPlux.update_element(wtx, from, 2, pubkey)
        DetsPlux.update_element(wtx, from, 3, sig_type)
    end
  end
end
