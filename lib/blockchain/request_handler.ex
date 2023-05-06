defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.{Events, Utils}
  alias Ippan.Request.Source

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
      event = Events.lookup(type)

      # if event == :undefined or event.system == true,
      #   do: raise(IppanError, "invalid type")

      hash = compute_hash(type, timestamp, from, args)
      size = Utils.estimate_size(request)

      # Check if the request is already in process or if there is a similar one for another account, select the correct request
      # if event.parallel, do: :l1, else: :l2
      hlist_name = :l1
      hlist_key = {event.base, hd(args)}
      # HashList.lookup!(hlist_name, hlist_key, hash, timestamp)

      # fetch account data
      account =
        case event.auth_type do
          0 -> from
          _ -> AccountStore.lookup(from)
        end

      ret =
        case event.auth_type do
          0 ->
            # build source
            source = %Source{
              hash: hash,
              account: account,
              event: event,
              timestamp: timestamp,
              size: size
            }

            # call function
            do_call(source, args)

          1 ->
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
            do_call(source, args)

          2 ->
            # hash verification
            <<seed::bytes-size(32), new_pkhash::bytes-size(32), new_hmac::bytes-size(16)>> =
              signature

            <<last_hash::bytes-size(32), pkhash::bytes-size(32), pkhash2::bytes-size(32),
              lhmac::bytes-size(16)>> = Map.get(account, :auth_hash)

            # verify hash and mac signature
            if not compare_hash(seed, pkhash), do: raise("Invalid hash verify")

            if not compare_mac(seed, last_hash, lhmac),
              do: raise("Invalid mac verify")

            # update account
            new_auth_hash = hash <> pkhash2 <> new_pkhash <> new_hmac
            # account = %{account | auth_hash: new_auth_hash}
            # DetsPlus.insert(:account, account)
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
            do_call(source, args)
        end

      case ret do
        {:error, _msg} = error ->
          error

        {:continue, fallback} ->
          HashList.insert(hlist_name, {hlist_key, {timestamp, hash, fallback}})
          RequestStore.insert(hash, request)
          {:ok, hash}

        _ ->
          HashList.insert(hlist_name, {hlist_key, {timestamp, hash, nil}})
          RequestStore.insert(hash, request)
          {:ok, hash}
      end
    rescue
      e in [IppanError] ->
        {:error, e.message}

      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
        {:error, "Invalid operation"}
    end
  end

  defmacrop default_hash(data) do
    quote do
      Blake3.hash(unquote(data))
    end
  end

  defmacrop default_hash_mac(data) do
    quote do
      <<_::bytes-size(16), rest::binary>> = Blake3.hash(unquote(data))
      rest
    end
  end

  @spec compute_hash(pos_integer(), pos_integer(), binary(), list()) :: binary()
  def compute_hash(type, timestamp, from, args) do
    str = Enum.reduce(args, "", fn x, acc -> :binary.list_to_bin([acc, x]) end)

    :binary.list_to_bin(["#{type}#{timestamp}", from, str])
    |> default_hash()
  end

  # defp check_hashlist!(_pid, {_base, nil}, _hash, _timestamp), do: :ok

  # defp check_hashlist!(pid, hlist_key, hash, timestamp) do
  #   case HashList.lookup(pid, hlist_key) do
  #     nil ->
  #       :ok

  #     {_, xhash} when hash == xhash ->
  #       raise IppanError, "Already exists"

  #     {old_timestamp, old_hash, fallback} ->
  #       cond do
  #         old_timestamp < timestamp ->
  #           raise IppanError, "Invalid operation"

  #         old_hash < hash ->
  #           raise IppanError, "Invalid operation"

  #         true ->
  #           case fallback do
  #             {fun, args} ->
  #               apply(Ippan.Func.Fallback, fun, args)

  #             _ ->
  #               :ok
  #           end
  #       end

  #     _ ->
  #       raise IppanError, "Invalid operation"
  #   end
  # end

  defp compare_hash(seed, pkhash) do
    default_hash(seed) == pkhash
  end

  defp compare_mac(seed, lhash, lhmac) do
    mac = :crypto.mac(@mac_algorithm, seed, lhash)

    lhmac == default_hash_mac(mac)
  end

  defp do_call(source, args) do
    apply(source.event.mod, source.event.fun, :lists.merge([source], args))
  end
end
