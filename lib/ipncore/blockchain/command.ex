defmodule Ippan.Command do
  alias Ippan.Events

  # @libsecp256k1 ExSecp256k1.Impl
  @json Application.compile_env(:ipncore, :json)

  @spec valid!(binary, binary, non_neg_integer(), binary, non_neg_integer()) :: any
  def valid!(hash, msg, size, sig_with_flag, node_validator_id) do
    [type, timestamp, from | args] = @json.decode!(msg)

    # if :os.system_time(:millisecond) < timestamp,
    # do: raise(IppanError, "Invalid timestamp")

    event = %{auth: true, validator: valid_validator, deferred: deferred} = Events.lookup(type)

    {wallet_pubkey, wallet_validator} =
      case valid_validator do
        true ->
          {_id, wallet_pubkey, wallet_validator, _created_at} = WalletStore.lookup(from)
          if wallet_validator != node_validator_id, do: raise(IppanError, "Invalid validator")
          {wallet_pubkey, wallet_validator}

        _false ->
          {_id, wallet_pubkey, wallet_validator, _created_at} = WalletStore.lookup(from)
          {wallet_pubkey, wallet_validator}
      end

    <<sig_flag::bytes-size(1), signature::binary>> = sig_with_flag
    chech_signature!(sig_flag, signature, hash, wallet_pubkey)

    result =
      case deferred do
        false ->
          [
            hash,
            timestamp,
            type,
            from,
            wallet_validator,
            node_validator_id,
            :erlang.term_to_binary(args),
            msg,
            sig_with_flag,
            size
          ]

        _true ->
          key = hd(args) |> to_string()

          [
            hash,
            timestamp,
            key,
            type,
            from,
            wallet_validator,
            node_validator_id,
            :erlang.term_to_binary(args),
            msg,
            sig_with_flag,
            size
          ]
      end

    {event, result}
  end

  # ======================================================

  def handle!(hash, type, timestamp, account_id, validator_id, node_id, size, args, round) do
    case Events.lookup(type) do
      %{deferred: false, mod: module, fun: fun} ->
        source = %{
          id: account_id,
          type: type,
          validator: validator_id,
          node: node_id,
          hash: hash,
          # round: round,
          timestamp: timestamp,
          size: size
        }

        apply(module, fun, [source | args])

      # deferred transactions
      %{mod: module, before: before} ->
        source = %{
          id: account_id,
          type: type,
          key: hd(args) |> to_string,
          validator: validator_id,
          node: node_id,
          hash: hash,
          round: round,
          timestamp: timestamp,
          size: size
        }

        apply(module, before, [source | args])
    end
  end

  # only deferred transactions
  def handle_post!(hash, type, timestamp, account_id, validator_id, node_id, size, args) do
    %{mod: module, fun: fun} = Events.lookup(type)

    source = %{
      id: account_id,
      type: type,
      key: hd(args) |> to_string,
      validator: validator_id,
      node: node_id,
      hash: hash,
      timestamp: timestamp,
      size: size
    }

    apply(module, fun, [source | args])
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
  # defp chech_signature!("2", signature, hash, wallet_pubkey) do
  #   if @libsecp256k1.verify(hash, signature, wallet_pubkey) !=
  #        :ok,
  #      do: raise(IppanError, "Invalid signature verify")
  # end

  defp chech_signature!(_, _signature, _hash, _wallet_pubkey) do
    raise(IppanError, "Signature type not supported")
  end
end
