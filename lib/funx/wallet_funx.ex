defmodule Ippan.Funx.Wallet do
  require BalanceStore
  alias Ippan.{Address, Utils}

  def new(_, pubkey, validator_id, sig_type) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)
    tx = DetsPlux.tx(:wallet)
    DetsPlux.put(tx, {id, pubkey, validator_id, sig_type})
  end

  def subscribe(
        %{id: from, validator: %{fa: fa, fb: fb, owner: vOwner}, size: size},
        validator_id
      ) do
    fees = Utils.calc_fees(fa, fb, size)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    case BalanceStore.pay_fee(from, vOwner, fees) do
      :error ->
        :error

      _ ->
        wdets = DetsPlux.get(:wallet)
        wtx = DetsPlux.tx(:wallet)
        DetsPlux.get_cache(wdets, wtx, from)
        DetsPlux.update_element(wtx, from, 2, validator_id)
    end
  end
end
