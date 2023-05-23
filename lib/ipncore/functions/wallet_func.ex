defmodule Ippan.Func.Wallet do
  alias Ippan.{Address, Wallet}

  def new(%{timestamp: timestamp}, validator_id, pubkey)
      when is_integer(validator_id) do
    pubkey = Fast64.decode64(pubkey)

    sig_type =
      case byte_size(pubkey) do
        33 ->
          0

        897 ->
          1

        _ ->
          raise IppanError, "Invalid pubkey size"
      end

    %Wallet{
      id: Address.hash(sig_type, pubkey),
      pubkey: pubkey,
      validator: validator_id,
      created_at: timestamp
    }
    |> Wallet.to_list()
    |> WalletStore.insert()
  end

  def subscribe(%{account: wallet, timestamp: timestamp, size: size}, validator_id) do
    cond do
      wallet.validator != validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        validator = ValidatorStore.lookup(wallet.validator)
        # fee amount is tx size
        :ok = BalanceStore.send_fees(wallet.id, validator.owner, size, timestamp)

        WalletStore.update(%{validator: validator_id}, id: wallet.id)
    end
  end
end
