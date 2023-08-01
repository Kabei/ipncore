defmodule Ippan.Func.Wallet do
  alias Phoenix.PubSub
  alias Ippan.{Address, Wallet}

  @token Application.compile_env(:ipncore, :token)
  @pubsub :verifiers
  @topic "wallet"

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

    wallet =
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

    wallet
    |> Wallet.to_list()
    |> WalletStore.insert()

    PubSub.broadcast(@pubsub, @topic, {"new", wallet})
  end

  def pre_sub(
        %{
          id: account_id,
          hash: hash,
          validator: validator_id,
          round: round,
          timestamp: timestamp,
          size: size
        },
        new_validator_id
      ) do
    cond do
      validator_id == new_validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(new_validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        :ok = BalanceStore.balance(account_id, @token, size)
        MessageStore.approve_df(round, timestamp, hash)
    end
  end

  def subscribe(
        %{id: account_id, timestamp: timestamp, size: size},
        new_validator_id
      ) do
    validator = ValidatorStore.lookup_map(new_validator_id)
    # fee amount is tx size
    :ok = BalanceStore.send_fees(account_id, validator.owner, size, timestamp)
    WalletStore.update(%{validator: new_validator_id}, id: account_id)

    PubSub.broadcast(
      @pubsub,
      @topic,
      {"sub", %{id: account_id, validator: new_validator_id}}
    )
  end
end
