defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.Events
  #  alias Ippan.Request.Source
  # alias Phoenix.PubSub

  @pubsub_server :pubsub
  @timeout Application.compile_env(:ipncore, :message_timeout)
  @libsecp256k1 ExSecp256k1.Impl
  @origin_from_client 0
  @origin_from_peer 1

  @type origin :: 0 | 1

  @spec handle(binary(), list(), non_neg_integer()) ::
          :ok | {:error, term()} | no_return()
  def handle(hash, msg, size) do
    try do
      [type, timestamp, args] = Jsonrs.decode!(msg)

      # if :os.timestamp() in (timestamp - @timeout)..(timestamp - @timeout),
      #   do: raise(IppanError, "Invalid timestamp")

      %{auth: false} = event = Events.lookup(type)

      source = %{
        hash: hash,
        event: event,
        timestamp: timestamp,
        size: size
      }

      apply(event.mod, event.fun, [source | List.wrap(args)])

      MessageStore.insert([hash, msg, nil, size])
    rescue
      e in [IppanError] ->
        {:error, e.message}

      MatchError ->
        # Logger.info(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end

  @spec handle(binary(), String.t(), non_neg_integer(), binary() | nil, origin()) ::
          :ok | {:error, term()} | no_return()
  def handle(hash, msg, size, sig_with_flag, origin) do
    try do
      [type, timestamp, from, args] = Jsonrs.decode!(msg)

      # if :os.timestamp() in (timestamp - @timeout)..(timestamp - @timeout),
      #   do: raise(IppanError, "Invalid timestamp")

      %{auth: true} = event = Events.lookup(type)

      [wallet_pubkey, wallet_validator] =
        case origin do
          0 ->
            WalletStore.lookup(from)

          1 ->
            WalletStore.lookup(from)
            # {:ok, [data]} =
            #   WalletStore.execute_prepare(:validator, [from, Default.validator_id()])

            # data
        end

      <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag

      IO.inspect("sig_flag")
      IO.inspect(sig_flag)

      case sig_flag do
        "0" ->
          # verify secp256k1 signature
          IO.inspect(wallet_pubkey)
          {:ok, pub} = ExSecp256k1.Impl.public_key_decompress(wallet_pubkey)
          # IO.inspect(signature)
          # IO.inspect(byte_size(signature))

          if @libsecp256k1.verify(hash, signature, pub) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        "1" ->
          # verify falcon-512 signature
          if Falcon.verify(hash, signature, wallet_pubkey) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        _ ->
          raise(IppanError, "Signature type not supported")
      end

      source = %{
        hash: hash,
        account: %{id: from, validator: wallet_validator},
        event: event,
        timestamp: timestamp,
        sig_type: sig_flag,
        size: size
      }

      # call function
      apply(event.mod, event.fun, [source | List.wrap(args)])

      MessageStore.insert([hash, msg, signature, size])
    rescue
      e in [IppanError] ->
        {:error, e.message}

      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end
end
