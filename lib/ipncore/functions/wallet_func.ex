defmodule Ippan.Func.Wallet do
  alias Ippan.{Address, Wallet}

  @token Default.token()

  def pre_new(%{timestamp: timestamp, hash: hash, round: round}, pubkey, validator_id)
      when is_integer(validator_id) do
    pubkey = Fast64.decode64(pubkey)

    case byte_size(pubkey) do
      32 ->
        :ok

      65 ->
        :ok

      897 ->
        :ok

      _ ->
        raise IppanError, "Invalid pubkey size"
    end

    MessageStore.approve_df(round, timestamp, hash)
  end

  def new(%{timestamp: timestamp}, pubkey, validator_id) do
    pubkey = Fast64.decode64(pubkey)

    case byte_size(pubkey) do
      32 ->
        %Wallet{
          id: Address.hash(0, pubkey),
          pubkey: pubkey,
          validator: validator_id,
          created_at: timestamp
        }

      65 ->
        %Wallet{
          id: Address.hash(2, pubkey),
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

  def pre_sub(
        %{id: account_id, validator: validator_id, timestamp: _timestamp, size: size},
        new_validator_id
      ) do
    cond do
      validator_id == new_validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        case ValidatorStore.lookup([new_validator_id]) do
          nil ->
            raise IppanError, "Validator not exists"

          _ ->
            # fee amount is tx size
            :ok = BalanceStore.balance(account_id, @token, size)
            WalletStore.update(%{validator: validator_id}, id: account_id)
        end
    end
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
