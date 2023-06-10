defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.Events
  # alias Phoenix.PubSub

  @pubsub_server :pubsub
  @timeout Application.compile_env(:ipncore, :message_timeout)
  @libsecp256k1 ExSecp256k1.Impl

  defmacrop check_timestamp!(timestamp) do
    quote do
      if :os.system_time(:millisecond) not in (unquote(timestamp) - @timeout)..(unquote(timestamp) +
                                                                                  @timeout),
         do: raise(IppanError, "Invalid timestamp")
    end
  end

  defmacrop run!(event, hash, msg, signature, size, from, wallet_validator, timestamp, args) do
    quote do
      case MessageStore.insert_sync([
             unquote(hash),
             unquote(msg),
             unquote(signature),
             unquote(size)
           ]) do
        1 ->
          source = %{
            hash: unquote(hash),
            id: unquote(from),
            validator: unquote(wallet_validator),
            timestamp: unquote(timestamp),
            size: unquote(size)
          }

          # call function
          apply(unquote(event).mod, unquote(event).fun, [source | unquote(args)])

        _ ->
          raise IppanError, "Invalid hash transaction"
      end
    end
  end

  defmacrop run_import!(
              event,
              hash,
              size,
              from,
              wallet_validator,
              timestamp,
              args
            ) do
    quote do
      source = %{
        hash: unquote(hash),
        id: unquote(from),
        validator: unquote(wallet_validator),
        timestamp: unquote(timestamp),
        size: unquote(size)
      }

      # call function
      apply(unquote(event).mod, unquote(event).fun, [source | unquote(args)])
    end
  end

  @spec valid!(binary, binary, non_neg_integer()) :: any
  def valid!(hash, msg, size) do
    [type, timestamp | _args] = Jason.decode!(msg)

    check_timestamp!(timestamp)

    %{auth: false} = Events.lookup(type)

    if MessageStore.insert_sync([hash, type, timestamp, nil, nil, nil, msg, nil, size]) != 1,
      do: raise(IppanError, "Invalid hash transaction")
  end

  @spec valid!(binary, binary, non_neg_integer(), binary, non_neg_integer()) :: any
  def valid!(hash, msg, size, sig_with_flag, node_validator) do
    [type, timestamp, from | args] = Jason.decode!(msg)

    check_timestamp!(timestamp)

    %{auth: true, validator: valid_validator} = Events.lookup(type)

    {wallet_pubkey, wallet_validator} =
      case valid_validator do
        true ->
          [_, wallet_pubkey, wallet_validator] = WalletStore.lookup([from])
          if wallet_validator != node_validator, do: raise(IppanError, "Invalid validator")
          {wallet_pubkey, wallet_validator}

        false ->
          [_, wallet_pubkey, _wallet_validator] = WalletStore.lookup([from])
          {wallet_pubkey, nil}
      end

    <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag
    chech_signature!(sig_flag, signature, hash, wallet_pubkey)

    if MessageStore.insert_sync([
         hash,
         type,
         timestamp,
         from,
         wallet_validator,
         args,
         :erlang.term_to_binary(msg),
         sig_with_flag,
         size
       ]) != 1,
       do: raise(IppanError, "Invalid hash transaction")
  end

  # ======================================================

  @spec handle!(binary(), list(), non_neg_integer()) ::
          any()
  def handle!(hash, type, timestamp, account_id, validator_id, size, args) do
    event = Events.lookup(type)

    source = %{
      account: account_id,
      validator: validator_id,
      hash: hash,
      # event: event,
      timestamp: timestamp,
      size: size
    }

    apply(event.mod, event.fun, [source | args])
  end

  # def handle_peer!(hash, msg, size, sig_with_flag) do
  #   [type, timestamp, from | args] = Jason.decode!(msg)

  #   check_timestamp!(timestamp)

  #   %{auth: true} = event = Events.lookup(type)

  #   [_, wallet_pubkey, wallet_validator] = WalletStore.lookup([from])

  #   <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag
  #   chech_signature!(sig_flag, signature, hash, wallet_pubkey)

  #   run!(event, hash, msg, signature, size, from, wallet_validator, timestamp, args)
  # end

  # def handle_import!(hash, msg, size) do
  #   [type, timestamp, from | args] = Jason.decode!(msg)

  #   %{auth: true} = event = Events.lookup(type)

  #   [_, _wallet_pubkey, wallet_validator] = WalletStore.lookup([from])

  #   run_import!(event, hash, size, from, wallet_validator, timestamp, args)
  # end

  # check signature by type
  # verify secp256k1 signature
  defp chech_signature!("0", signature, hash, wallet_pubkey) do
    if @libsecp256k1.verify(hash, signature, wallet_pubkey) !=
         :ok,
       do: raise(IppanError, "Invalid signature verify")
  end

  # verify falcon-512 signature
  defp chech_signature!("1", signature, hash, wallet_pubkey) do
    if Falcon.verify(hash, signature, wallet_pubkey) != :ok,
      do: raise(IppanError, "Invalid signature verify")
  end

  # verify ed25519 signature
  defp chech_signature!("2", signature, hash, wallet_pubkey) do
    if Ed25519Blake2b.Native.verify(
         hash,
         signature,
         wallet_pubkey
       ) != :ok,
       do: raise(IppanError, "Invalid signature verify")
  end

  defp chech_signature!(_, _signature, _hash, _wallet_pubkey) do
    raise(IppanError, "Signature type not supported")
  end
end
