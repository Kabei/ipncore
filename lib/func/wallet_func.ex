defmodule Ippan.Func.Wallet do
  alias Ippan.Address
  require SqliteStore

  def subscribe(
        %{id: account_id, validator: validator, wallets: {wallet_dets, wallet_tx}},
        pubkey,
        validator_id,
        sig_type
      ) do
    pubkey = Fast64.decode64(pubkey)
    id = Address.hash(sig_type, pubkey)

    cond do
      id != account_id ->
        raise IppanError, "Invalid sender"

      validator_id != validator.id ->
        raise IppanError, "Invalid validator"

      sig_type not in 0..1 ->
        raise IppanError, "Invalid signature type"

      byte_size(pubkey) > 897 ->
        raise IppanError, "Invalid pubkey size"

      DetsPlux.member_tx?(wallet_dets, wallet_tx, id) ->
        raise IppanError, "Wallet already exists"

      true ->
        :ok
    end
  end

  def unsubscribe(
        %{
          validator: validator,
          conn: conn,
          stmts: stmts
        },
        new_validator_id
      ) do
    cond do
      validator.id == new_validator_id ->
        raise IppanError, "Already subscribe"

      not SqliteStore.exists?(conn, stmts, "exists_validator", new_validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        :ok
    end
  end
end
