defmodule Ippan.Funx.Wallet do
  alias Ippan.Address

  def new(_, pubkey, validator_id, sig_type) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)
    tx = DetsPlux.tx(:wallet)
    DetsPlux.put(tx, id, {pubkey, validator_id})
  end

  def subscribe(%{id: account_id}, validator_id) do
    tx = DetsPlux.tx(:wallet)
    [_, {pubkey, _vid}] = :ets.lookup(tx, account_id)
    DetsPlux.put(tx, account_id, {pubkey, validator_id})
  end
end
