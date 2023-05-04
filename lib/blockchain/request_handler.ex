defmodule Ippan.RequestHandler do
  alias Ippan.{Events, Utils}
  alias Ippan.Request.Source

  @mac_algorithm :poly1305

  @type request_tuple ::
          {non_neg_integer(), non_neg_integer(), String.t(), [any()], binary()}
  # @type request_list :: list()
  @type hash :: binary()

  # @spec handle(request_tuple) :: {:ok, hash()} | {:error, term()}
  # def handle([type, timestamp, from, args, signature]),
  #   do: handle({type, timestamp, from, args, signature} = request)

  def handle({type, timestamp, from, args, signature} = request) do
    try do
      event = Events.lookup(type)

      if event == :undefined or event.system == true,
        do: raise(IppanError, "invalid type")

      hash = compute_hash(type, timestamp, from, args)

      # Check if the request is already in process or if there is a similar one for another account, select the correct request
      hlist_atom = if event.parallel, do: :l1, else: :l2
      id = List.first(args)
      hlist_key = {event.base, id}
      check_hashlist!(hlist_atom, hlist_key, hash, timestamp)

      # fetch account data
      account =
        case event.auth_type do
          0 ->
            nil

          _ ->
            DetsPlus.lookup(:account, from)
        end

      size = Utils.estimate_size(request)

      # build source
      source = %Source{
        hash: hash,
        account: account,
        event: event,
        timestamp: timestamp,
        size: size
      }

      ret =
        case event.auth_type do
          0 ->
            # call function
            do_call(source, args)

          1 ->
            # verify falcon signature
            if Falcon.verify(hash, signature, account.pubkey) == :ok,
              do: raise("Invalid signature verify")

            # call function
            do_call(source, args)

          2 ->
            # hash verification
            <<seed::bytes-size(32), new_pkhash::bytes-size(32), new_hmac::bytes-size(16)>> =
              signature

            <<lhash::bytes-size(32), pkhash::bytes-size(32), lhmac::bytes-size(16)>> =
              account.auth_hash

            # verify hash and mac signature
            if not compare_hash(seed, pkhash), do: raise("Invalid hash verify")

            if not compare_mac(seed, lhash, lhmac),
              do: raise("Invalid mac verify")

            # update account
            new_auth_hash = hash <> new_pkhash <> new_hmac
            account = %{account | auth_hash: new_auth_hash}
            DetsPlus.insert(:account, account)

            # call function
            do_call(%{source | account: account}, args)
        end

      case ret do
        {:error, _msg} = error ->
          error

        {:continue, fallback} ->
          HashList.insert(hlist_atom, {hlist_key, {timestamp, hash, fallback}})
          {:ok, hash}

        _ ->
          HashList.insert(hlist_atom, {hlist_key, {timestamp, hash, nil}})
          {:ok, hash}
      end
    rescue
      e in [IppanError] ->
        {:error, e.message}

      _ ->
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
    str = Enum.map_join(args, "", &to_string(&1))

    default_hash("#{type}#{timestamp}#{from}#{str}")
  end

  defp check_hashlist!(_pid, {_base, nil}, _hash, _timestamp), do: :ok

  defp check_hashlist!(pid, hlist_key, hash, timestamp) do
    case HashList.lookup(pid, hlist_key) do
      nil ->
        :ok

      {_, xhash} when hash == xhash ->
        raise IppanError, "Already exists"

      {old_timestamp, old_hash, fallback} ->
        cond do
          old_timestamp < timestamp ->
            raise IppanError, "Invalid operation"

          old_hash < hash ->
            raise IppanError, "Invalid operation"

          true ->
            case fallback do
              {module, fun, args} ->
                apply(module, fun, args)

              _ ->
                :ok
            end
        end

      _ ->
        raise IppanError, "Invalid operation"
    end
  end

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
