defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.Func.Wallet
  alias Ippan.{Wallet, Events}
  #  alias Ippan.Request.Source
  # alias Phoenix.PubSub

  @pubsub_server :pubsub
  @timeout Application.compile_env(:ipncore, :message_timeout)
  @libsecp256k1 ExSecp256k1.Impl

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

      apply(event.mod, event.fun, [source | args])

      MessageStore.insert([hash, msg, nil, size])
    rescue
      e in [IppanError] ->
        {:error, e.message}

      MatchError ->
        # Logger.info(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end

  @spec handle(binary(), String.t(), non_neg_integer(), binary() | nil) ::
          :ok | {:error, term()} | no_return()
  def handle(hash, msg, size, sig_with_flag) do
    try do
      [type, timestamp, from, args] = Jsonrs.decode!(msg)

      # if :os.timestamp() in (timestamp - @timeout)..(timestamp - @timeout),
      #   do: raise(IppanError, "Invalid timestamp")

      %{auth: true} = event = Events.lookup(type)

      wallet = WalletStore.execute_prepare(:validator, [from, Global.get(:validator_id)])

      if wallet == [], do: raise(IppanError, "Invalid wallet ID or not subscribe")

      wallet = Wallet.to_map(wallet)

      <<sig_flag::8, signature::binary>> = sig_with_flag

      case sig_flag do
        0 ->
          # verify falcon signature
          if Falcon.verify(hash, signature, wallet.pubkey) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        1 ->
          if @libsecp256k1.verify(hash, signature, wallet.pubkey) != true,
            do: raise(IppanError, "Invalid signature verify")

        _ ->
          raise(IppanError, "Signature type not supported")
      end

      case event.parallel do
        true ->
          GlobalRequestStore.insert([type, hd(args), hash, timestamp])
          :noreply

        false ->
          # build source
          source = %{
            hash: hash,
            account: wallet,
            event: event,
            timestamp: timestamp,
            sig_type: sig_flag,
            size: size
          }

          # call function
          apply(event.mod, event.fun, [source | args])
      end

      MessageStore.insert([hash, msg, signature, size])
    rescue
      e in [IppanError] ->
        {:error, e.message}

      MatchError ->
        # Logger.info(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end
end
