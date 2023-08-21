defmodule Ippan.Func.Wallet do
  require Global
  # alias Phoenix.PubSub
  alias Ippan.{Address, Wallet}

  # @pubsub :cluster
  # @topic "wallet"

  def pre_sub(%{timestamp: timestamp, hash: hash, round: round}, pubkey, validator_id, sig_type)
      when is_integer(validator_id) do
    pubkey = Fast64.decode64(pubkey)

    cond do
      sig_type not in 0..2 ->
        raise IppanError, "Invalid signature type"

      byte_size(pubkey) > 897 ->
        raise IppanError, "Invalid pubkey size"

      WalletStore.exists?(Address.hash(sig_type, pubkey)) ->
        raise IppanError, "Already exists"

      true ->
        MessageStore.approve_df(round, timestamp, hash)
    end
  end

  def subscribe(%{timestamp: timestamp}, pubkey, validator_id) do
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

    # PubSub.local_broadcast(@pubsub, @topic, {"subscribe", wallet})
  end

  def unsubscribe(
        %{
          id: account_id,
          validator: validator_id
        },
        new_validator_id
      ) do
    cond do
      validator_id == new_validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(new_validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        WalletStore.delete([account_id])

        # if Global.validator_id() == new_validator_id do
        #   PubSub.local_broadcast(
        #     @pubsub,
        #     @topic,
        #     {"unsubscribe", %{id: account_id, validator: new_validator_id}}
        #   )
        # end
    end
  end
end
