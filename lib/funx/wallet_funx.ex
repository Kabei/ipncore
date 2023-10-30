defmodule Ippan.Funx.Wallet do
  alias Ippan.Address

  def new(_, pubkey, validator_id, sig_type) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)
    tx = DetsPlux.tx(:wallet)
    DetsPlux.put(tx, {id, pubkey, validator_id})
  end

  def subscribe(%{id: account_id}, validator_id) do
    dets = DetsPlux.get(:wallet)
    tx = DetsPlux.tx(:wallet)
    DetsPlux.get_cache(dets, tx, account_id)
    DetsPlux.update_element(tx, account_id, 2, validator_id)
  end
end
