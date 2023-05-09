defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.{Events, Utils}
  alias Ippan.Request.Source
  alias Phoenix.PubSub

  @pubsub_server :pubsub
  @mac_algorithm :poly1305

  @type request_tuple ::
          {non_neg_integer(), non_neg_integer(), binary(), [any()], binary()}
  # @type request_list :: list()
  @type hash :: binary()

  # def handle([type, timestamp, from, args, signature]),
  #   do: handle({type, timestamp, from, args, signature} = request)

  @spec handle(request_tuple) :: {:ok, hash()} | {:error, term()}
  def handle({type, timestamp, from, args, signature} = request) do
    try do
      %{base: event_base} = event = Events.lookup(type)
      hash = compute_hash(type, timestamp, from, args)

      hlist_name = :l1
      hlist_key = {event_base, List.first(args)}
      HashList.lookup!(hlist_name, hlist_key, hash, timestamp)

      size = Utils.estimate_size(request)

      # Check if the request is already in process or if there is a similar one for another account, select the correct request
      # if event.parallel, do: :l1, else: :l2

      case event.auth_type do
        0 ->
          # build source
          source = %Source{
            hash: hash,
            account: from,
            event: event,
            timestamp: timestamp,
            size: size
          }

          # call function
          # do_call(source, args)
          apply(event.mod, event.fun, [source | args])

        1 ->
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
            do: raise("Invalid signature verify")

          # call function
          # do_call(source, args)
          apply(event.mod, event.fun, [source | args])

        2 ->
          account = AccountStore.lookup(:validator, [from, Default.validator_id()])

          if is_nil(account), do: raise(IppanError, "Invalid account ID or Validator ID")
          # hash verification
          <<seed::bytes-size(32), new_pkhash::bytes-size(32), new_hmac::bytes-size(16)>> =
            signature

          new_auth_hash =
            case Map.get(account, :auth_hash) do
              # no last hash
              <<pkhash::bytes-size(32), pkhash2::bytes-size(32), lhmac::bytes-size(16)>> ->
                # verify hash signature
                if not compare_hash(seed, pkhash),
                  do: raise(IppanError, "Invalid auth-hash verify")

                # verify mac signature
                if not compare_mac(seed, "", lhmac),
                  do: raise(IppanError, "Invalid auth-mac verify")

                [pkhash2, new_pkhash, new_hmac, hash]

              # last hash included
              <<pkhash::bytes-size(32), pkhash2::bytes-size(32), lhmac::bytes-size(16),
                last_hash::bytes-size(32)>> ->
                # verify hash signature
                if not compare_hash(seed, pkhash),
                  do: raise(IppanError, "Invalid auth-hash verify")

                # verify mac signature
                if not compare_mac(seed, last_hash, lhmac),
                  do: raise(IppanError, "Invalid auth-mac verify")

                [pkhash2, new_pkhash, new_hmac, hash]

              _ ->
                raise(IppanError, "Invalid auth-hash recorded")
            end
            |> :binary.list_to_bin()

          # update account
          AccountStore.update(%{auth_hash: new_auth_hash}, id: account.id)

          # build source
          source = %Source{
            hash: hash,
            account: account,
            event: event,
            timestamp: timestamp,
            size: size
          }

          # call function
          # do_call(source, args)
          apply(event.mod, event.fun, [source | args])
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

  defmacrop default_hash(data) do
    quote do
      Blake3.Native.hash(unquote(data))
    end
  end

  defmacrop default_hash_mac(data) do
    quote do
      <<_::bytes-size(16), rest::binary>> = Blake3.Native.hash(unquote(data))
      rest
    end
  end

  @spec compute_hash(pos_integer(), pos_integer(), binary(), list()) :: binary()
  def compute_hash(type, timestamp, from, args) do
    str =
      Enum.reduce(args, "#{type}#{timestamp}#{from}", fn x, acc ->
        :binary.list_to_bin([acc, x])
      end)

    default_hash(str)
  end

  defp compare_hash(seed, pkhash) do
    default_hash(seed) == pkhash
  end

  defp compare_mac(seed, lhash, lhmac) do
    mac = :crypto.mac(@mac_algorithm, seed, lhash)

    lhmac == default_hash_mac(mac)
  end

  # defp do_call(source, args) do
  #   apply(source.event.mod, source.event.fun, :lists.merge([source], args))
  # end
end
