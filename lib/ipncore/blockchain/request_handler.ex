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

  defmacrop chech_signature!(hash, sig_flag, signature, wallet_pubkey) do
    quote do
      # check signature type
      case unquote(sig_flag) do
        "0" ->
          # verify secp256k1 signature
          if @libsecp256k1.verify(unquote(hash), unquote(signature), unquote(wallet_pubkey)) !=
               :ok,
             do: raise(IppanError, "Invalid signature verify")

        "1" ->
          # verify falcon-512 signature
          if Falcon.verify(unquote(hash), unquote(signature), unquote(wallet_pubkey)) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        _ ->
          raise(IppanError, "Signature type not supported")
      end
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

  @spec handle!(binary(), list(), non_neg_integer()) ::
          any()
  def handle!(hash, msg, size) do
    [type, timestamp | args] = Jason.decode!(msg)

    check_timestamp!(timestamp)

    %{auth: false} = event = Events.lookup(type)

    source = %{
      hash: hash,
      # event: event,
      timestamp: timestamp,
      size: size
    }

    apply(event.mod, event.fun, [source | args])

    if MessageStore.insert_sync([hash, msg, nil, size]) != 1,
      do: raise(IppanError, "Invalid hash transaction")
  end

  @spec handle!(binary(), String.t(), non_neg_integer(), binary() | nil) ::
          any()

  def handle!(hash, msg, size, sig_with_flag) do
    [type, timestamp, from | args] = Jason.decode!(msg)

    check_timestamp!(timestamp)

    %{auth: true} = event = Events.lookup(type)

    [_, wallet_pubkey, wallet_validator] = WalletStore.lookup([from])

    <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag
    chech_signature!(hash, sig_flag, signature, wallet_pubkey)

    run!(event, hash, msg, signature, size, from, wallet_validator, timestamp, args)
  end

  def handle_peer!(hash, msg, size, sig_with_flag) do
    [type, timestamp, from | args] = Jason.decode!(msg)

    check_timestamp!(timestamp)

    %{auth: true} = event = Events.lookup(type)

    [_, wallet_pubkey, wallet_validator] = WalletStore.lookup([from])

    <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag
    chech_signature!(hash, sig_flag, signature, wallet_pubkey)

    run!(event, hash, msg, signature, size, from, wallet_validator, timestamp, args)
  end

  def handle_import!(hash, msg, size) do
    [type, timestamp, from | args] = Jason.decode!(msg)

    %{auth: true} = event = Events.lookup(type)

    [_, _wallet_pubkey, wallet_validator] = WalletStore.lookup([from])

    run_import!(event, hash, size, from, wallet_validator, timestamp, args)
  end
end
