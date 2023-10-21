defmodule Ippan.Funx.Wallet do
  alias Ippan.Address

  def subscribe(_, pubkey, validator_id, sig_type) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)
    tx = DetsPlux.tx(:wallet)
    DetsPlux.put(tx, id, {pubkey, validator_id})
  end

  def unsubscribe(%{id: account_id}) do
    tx = DetsPlux.tx(:wallet)
    DetsPlux.delete(tx, account_id)
  end
end
