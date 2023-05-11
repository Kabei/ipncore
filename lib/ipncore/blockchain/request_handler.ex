defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.Func.Account
  alias Ippan.{Events, Utils}
  alias Ippan.Request.Source
  alias Phoenix.PubSub

  @pubsub_server :pubsub
  # @mac_algorithm :poly1305

  # @type request_tuple ::
  #         {non_neg_integer(), non_neg_integer(), binary(), [any()], binary()}
  # @type request_list :: list()
  # @type hash :: binary()

  # def handle([type, timestamp, from, args, sig]),
  #   do: handle({type, timestamp, from, args, sig} = request)

  # def pending(hash, msg, size, sig) do
  #   [type, timestamp, from, args]

  #   event = Events.lookup(type)

  # end

  @spec handle(binary(), list(), non_neg_integer()) ::
          :ok | {:error, term()} | no_return()
  def handle(hash, [type, timestamp, from, args], size) do
    %{auth: false} = event = Events.lookup(type)

    source = %Source{
      hash: hash,
      account: from,
      event: event,
      timestamp: timestamp,
      size: size
    }

    apply(event.mod, event.fun, [source | args])
  end

  @spec handle(binary(), list(), non_neg_integer(), binary() | nil) ::
          :ok | {:error, term()} | no_return()
  def handle(hash, [type, timestamp, from, args] = request, size, sig) do
    try do
      %{base: event_base, auth: false} = event = Events.lookup(type)
      # hash = compute_hash(type, timestamp, from, args)

      # hlist_name = :l1
      # hlist_key = {event_base, List.first(args)}
      # HashList.lookup!(hlist_name, hlist_key, hash, timestamp)

      # size = Utils.estimate_size(request)

      # Check if the request is already in process or if there is a similar one for another account, select the correct request
      # if event.parallel, do: :l1, else: :l2
      account = AccountStore.lookup(:validator, [from, Global.get(:validator_id)])

      if is_nil(account), do: raise(IppanError, "Invalid account ID or not subscribe")

      # build source
      source = %Source{
        hash: hash,
        account: Ippan.Account.to_map(account),
        event: event,
        timestamp: timestamp,
        size: size
      }

      <<sig_flag::bytes-size(1), signature::binary>> = sig

      case sig_flag do
        0 ->
          account = AccountStore.lookup(from)
          # build source
          source = %Source{
            hash: hash,
            account: account,
            event: event,
            timestamp: timestamp,
            size: size
          }

          # verify falcon signature
          if Falcon.verify(hash, signature, account.pubkey) == :ok,
            do: raise(IppanError, "Invalid signature verify")

          # call function
          # do_call(source, args)
          apply(event.mod, event.fun, [source | args])

        _ ->
          raise(IppanError, "Signature type not supported")
      end
      |> case do
        :ok ->
          HashList.insert(hlist_name, {hlist_key, {timestamp, hash}})
          RequestStore.insert(hash, request)
          {:ok, hash}

        {:notify, data} ->
          PubSub.broadcast(@pubsub_server, event.name, %{event: event.name, data: data})

        {:continue, fallback} ->
          HashList.insert(hlist_name, {hlist_key, {timestamp, hash, fallback}})
          RequestStore.insert(hash, request)
          {:ok, hash}

        1 ->
          {:ok, hash}

        error ->
          # Logger.debug("error: #{inspect(error)}")
          error
      end
    rescue
      e in [IppanError] ->
        {:error, e.message}

      MatchError ->
        # Logger.info(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end

  # defmacrop default_hash(data) do
  #   quote do
  #     Blake3.Native.hash(unquote(data))
  #   end
  # end

  # defmacrop default_hash_mac(data) do
  #   quote do
  #     <<_::bytes-size(16), rest::binary>> = Blake3.Native.hash(unquote(data))
  #     rest
  #   end
  # end

  # @spec compute_hash(pos_integer(), pos_integer(), binary(), list()) :: binary()
  # def compute_hash(type, timestamp, from, args) do
  #   str =
  #     Enum.reduce(args, "#{type}#{timestamp}#{from}", fn x, acc ->
  #       :binary.list_to_bin([acc, x])
  #     end)

  #   default_hash(str)
  # end

  # defp compare_hash(seed, pkhash) do
  #   default_hash(seed) == pkhash
  # end

  # defp compare_mac(seed, lhash, lhmac) do
  #   mac = :crypto.mac(@mac_algorithm, seed, lhash)

  #   lhmac == default_hash_mac(mac)
  # end

  # defp do_call(source, args) do
  #   apply(source.event.mod, source.event.fun, :lists.merge([source], args))
  # end
end
