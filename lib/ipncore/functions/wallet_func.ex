defmodule Ippan.Func.Wallet do
  alias Ippan.{Address, Wallet}

  def new(%{timestamp: timestamp}, validator_id, pubkey)
      when is_integer(validator_id) do
    pubkey = Fast64.decode64(pubkey)
    # {:ok, pub} = ExSecp256k1.Impl.public_key_decompress(pubkey)

    case byte_size(pubkey) do
      33 ->
        %Wallet{
          id: Address.hash(0, pubkey),
          pubkey: pubkey,
          validator: validator_id,
          created_at: timestamp
        }

      897 ->
        %Wallet{
          id: Address.hash(1, pubkey),
          pubkey: pubkey,
          validator: validator_id,
          created_at: timestamp
        }

      _ ->
        raise IppanError, "Invalid pubkey size"
    end
    |> Wallet.to_list()
    |> WalletStore.insert()
  end

  def subscribe(
        %{id: account_id, validator: validator_id, timestamp: timestamp, size: size},
        new_validator_id
      ) do
    cond do
      validator_id == new_validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        validator = ValidatorStore.lookup([new_validator_id])
        # fee amount is tx size
        :ok = BalanceStore.send_fees(account_id, validator.owner, size, timestamp)

        WalletStore.update(%{validator: validator_id}, id: account_id)
    end
  end
end
