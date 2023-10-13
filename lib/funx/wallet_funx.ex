defmodule Ippan.Funx.Wallet do
  alias Ippan.Address

  def subscribe(
        %{wallets: {_wallet_dets, wallet_tx}},
        pubkey,
        validator_id,
        sig_type
      ) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)

    DetsPlux.put(wallet_tx, id, {pubkey, validator_id})
  end

  def unsubscribe(%{
        id: account_id,
        wallets: {_wallet_dets, wallet_tx}
      }) do
    DetsPlux.delete(wallet_tx, account_id)
  end
end
