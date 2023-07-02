defmodule Ippan.RequestHandler do
  require Logger
  alias Ippan.Events
  # alias Phoenix.PubSub

  # @timeout Application.compile_env(:ipncore, :message_timeout)
  @libsecp256k1 ExSecp256k1.Impl

  defmacrop check_timestamp!(timestamp) do
    quote do
      if :os.system_time(:millisecond) not in (unquote(timestamp) - @timeout)..(unquote(timestamp) +
                                                                                  @timeout),
         do: raise(IppanError, "Invalid timestamp")
    end
  end

  @spec valid!(binary, binary, non_neg_integer()) :: any
  def valid!(hash, msg, size) do
    [type, timestamp | args] = Jason.decode!(msg)
    IO.inspect(args)

    # check_timestamp!(timestamp)

    event = %{auth: false, deferred: deferred} = Events.lookup(type)

    result =
      case deferred do
        true ->
          IO.inspect(args)
          key = hd(args) |> to_string()

          [
            key,
            type,
            timestamp,
            hash,
            nil,
            Default.validator_id(),
            :erlang.term_to_binary(args),
            msg,
            nil,
            size
          ]

        false ->
          [
            timestamp,
            hash,
            type,
            nil,
            Default.validator_id(),
            :erlang.term_to_binary(args),
            msg,
            nil,
            size
          ]
      end

    {event, result}
  end

  @spec valid!(binary, binary, non_neg_integer(), binary, non_neg_integer()) :: any
  def valid!(hash, msg, size, sig_with_flag, node_validator) do
    [type, timestamp, from | args] = Jason.decode!(msg)

    # check_timestamp!(timestamp)

    event = %{auth: true, validator: valid_validator, deferred: deferred} = Events.lookup(type)

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

    result =
      case deferred do
        true ->
          key = hd(args) |> to_string()

          [
            key,
            type,
            timestamp,
            hash,
            from,
            wallet_validator,
            :erlang.term_to_binary(args),
            msg,
            sig_with_flag,
            size
          ]

        false ->
          [
            timestamp,
            hash,
            type,
            from,
            wallet_validator,
            :erlang.term_to_binary(args),
            msg,
            sig_with_flag,
            size
          ]
      end

    {event, result}
  end

  # ======================================================

  def handle!(hash, type, timestamp, account_id, validator_id, size, nil, round) do
    event = Events.lookup(type)

    source = %{
      id: account_id,
      type: type,
      validator: validator_id,
      hash: hash,
      round: round,
      timestamp: timestamp,
      size: size
    }

    case event do
      %{deferred: false} ->
        apply(event.mod, event.fun, [source])

      %{deferred: true} ->
        apply(event.mod, event.before, [source])
    end
  end

  def handle!(hash, type, timestamp, account_id, validator_id, size, args, round) do
    event = Events.lookup(type)

    case event do
      %{deferred: false} ->
        source = %{
          id: account_id,
          type: type,
          validator: validator_id,
          hash: hash,
          round: round,
          timestamp: timestamp,
          size: size
        }

        apply(event.mod, event.fun, [source | args])

      %{deferred: true} ->
        source = %{
          id: account_id,
          type: type,
          key: hd(args) |> to_string,
          validator: validator_id,
          hash: hash,
          round: round,
          timestamp: timestamp,
          size: size
        }

        apply(event.mod, event.before, [source | args])
    end
  end

  def handle_post!(hash, type, timestamp, account_id, validator_id, size, args) do
    event = Events.lookup(type)

    source = %{
      id: account_id,
      type: type,
      key: hd(args) |> to_string,
      validator: validator_id,
      hash: hash,
      timestamp: timestamp,
      size: size
    }

    apply(event.mod, event.fun, [source | args])
  end

  # check signature by type
  # verify ed25519 signature
  defp chech_signature!("0", signature, hash, wallet_pubkey) do
    if Cafezinho.Impl.verify(
         signature,
         hash,
         wallet_pubkey
       ) != :ok,
       do: raise(IppanError, "Invalid signature verify")
  end

  # verify falcon-512 signature
  defp chech_signature!("1", signature, hash, wallet_pubkey) do
    if Falcon.verify(hash, signature, wallet_pubkey) != :ok,
      do: raise(IppanError, "Invalid signature verify")
  end

  # verify secp256k1 signature
  defp chech_signature!("2", signature, hash, wallet_pubkey) do
    if @libsecp256k1.verify(hash, signature, wallet_pubkey) !=
         :ok,
       do: raise(IppanError, "Invalid signature verify")
  end

  defp chech_signature!(_, _signature, _hash, _wallet_pubkey) do
    raise(IppanError, "Signature type not supported")
  end
end
