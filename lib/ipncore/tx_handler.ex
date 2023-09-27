defmodule Ippan.TxHandler do
  alias Ippan.Wallet
  alias Ippan.Funcs
  alias Sqlite3NIF
  require SqliteStore

  @json Application.compile_env(:ipncore, :json)

  @spec valid!(reference, map, binary, binary, binary, integer, integer, map) :: list()
  def valid!(conn, stmts, hash, msg, signature, size, validator_node_id, validator) do
    [type, timestamp, from | args] = @json.decode!(msg)

    # if timestamp < :persistent_term.get(:time_expired, 0) or timestamp > :os.system_time(:millisecond) + 20000 do
    if timestamp < :persistent_term.get(:time_expired, 0) do
      raise(IppanError, "Invalid timestamp")
    end

    %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification} = Funcs.lookup(type)

    %{pubkey: wallet_pubkey} =
      case type_of_verification do
        # check from variable
        0 ->
          result =
            %{validator: wallet_validator} =
            SqliteStore.lookup_map(:wallet, conn, stmts, "get_wallet", from, Wallet)

          if wallet_validator != validator_node_id do
            raise IppanRedirectError, "#{validator_node_id}"
          end

          result

        # get first argument
        1 ->
          %{pubkey: args |> hd |> Fast64.decode64()}

        # check first argument
        2 ->
          key = hd(args)

          result =
            %{validator: wallet_validator} =
            SqliteStore.lookup_map(:wallet, conn, stmts, "get_wallet", key, Wallet)

          if wallet_validator != validator_node_id do
            raise IppanRedirectError, "#{wallet_validator}"
          end

          result
      end

    [sig_type, _] = String.split(from, "x", parts: 2)
    check_signature!(sig_type, signature, hash, wallet_pubkey)

    source = %{
      conn: conn,
      dets: :persistent_term.get(:dets_balance),
      id: from,
      hash: hash,
      size: size,
      stmts: stmts,
      timestamp: timestamp,
      type: type,
      validator: validator
    }

    apply(mod, fun, [source | args])

    case deferred do
      false ->
        [
          deferred,
          [
            hash,
            type,
            from,
            args,
            timestamp,
            [msg, signature],
            size
          ]
        ]

      _true ->
        key = hd(args) |> to_string()

        [
          deferred,
          [
            hash,
            type,
            key,
            from,
            args,
            timestamp,
            [msg, signature],
            size
          ]
        ]
    end
  end

  @spec valid_from_file!(reference, map, binary, binary, binary, integer, integer, map) :: list()
  def valid_from_file!(conn, stmts, hash, msg, signature, size, validator_node_id, validator) do
    [type, timestamp, from | args] = @json.decode!(msg)

    # if timestamp < :persistent_term.get(:time_expired, 0) or timestamp > :os.system_time(:millisecond) + 20000 do
    if timestamp < :persistent_term.get(:time_expired, 0) do
      raise(IppanError, "Invalid timestamp")
    end

    %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification} = Funcs.lookup(type)

    %{pubkey: wallet_pubkey} =
      case type_of_verification do
        # check from variable
        0 ->
          result =
            %{validator: wallet_validator} =
            SqliteStore.lookup_map(:wallet, conn, stmts, "get_wallet", from, Wallet)

          if wallet_validator != validator_node_id do
            raise IppanRedirectError, "#{validator_node_id}"
          end

          result

        # get first argument
        1 ->
          %{pubkey: args |> hd |> Fast64.decode64()}

        # check first argument
        2 ->
          key = hd(args)

          result =
            %{validator: wallet_validator} =
            SqliteStore.lookup_map(:wallet, conn, stmts, "get_wallet", key, Wallet)

          if wallet_validator != validator_node_id do
            raise IppanRedirectError, "#{wallet_validator}"
          end

          result
      end

    [sig_type, _] = String.split(from, "x", parts: 2)
    check_signature!(sig_type, signature, hash, wallet_pubkey)

    source = %{
      conn: conn,
      dets: :persistent_term.get(:dets_balance),
      hash: hash,
      size: size,
      stmts: stmts,
      timestamp: timestamp,
      validator: validator
    }

    apply(mod, fun, [source | args])

    case deferred do
      false ->
        [
          hash,
          type,
          from,
          args,
          timestamp,
          size
        ]

      _true ->
        key = hd(args) |> to_string()

        [
          hash,
          type,
          key,
          from,
          args,
          timestamp,
          size
        ]
    end
  end

  # check signature by type
  # verify ed25519 signature
  defp check_signature!("0", signature, hash, wallet_pubkey) do
    if Cafezinho.Impl.verify(
         signature,
         hash,
         wallet_pubkey
       ) != :ok,
       do: raise(IppanError, "Invalid signature verify")
  end

  # verify falcon-512 signature
  defp check_signature!("1", signature, hash, wallet_pubkey) do
    if Falcon.verify(hash, signature, wallet_pubkey) != :ok,
      do: raise(IppanError, "Invalid signature verify")
  end

  defp check_signature!(_, _signature, _hash, _wallet_pubkey) do
    raise(IppanError, "Signature type not supported")
  end

  defmacro handle_regular(
             conn,
             stmts,
             dets,
             validator,
             hash,
             type,
             from,
             args,
             size,
             timestamp,
             block_id
           ) do
    quote bind_quoted: [
            conn: conn,
            stmts: stmts,
            dets: dets,
            validator: validator,
            hash: hash,
            type: type,
            from: from,
            args: args,
            timestamp: timestamp,
            size: size,
            block_id: block_id
          ],
          location: :keep do
      %{fun: fun, modx: module} = Funcs.lookup(type)

      environment = %{
        conn: conn,
        block_id: block_id,
        stmts: stmts,
        dets: dets,
        id: from,
        type: type,
        validator: validator,
        hash: hash,
        timestamp: timestamp,
        size: size
      }

      apply(module, fun, [environment | args])
    end
  end

  # Dispute resolution in deferred transaction
  def insert_deferred(
        [hash, type, arg_key, account_id, args, timestamp, size],
        validator_id,
        block_id
      ) do
    key = {type, arg_key}
    body = [hash, account_id, validator_id, args, timestamp, size]

    case :ets.lookup(:dtx, key) do
      [] ->
        :ets.insert(:dtx, {key, body})

      [{_msg_key, xhash, xblock_id, _rest}] ->
        if hash < xhash or (hash == xhash and block_id < xblock_id) do
          :ets.insert(:dtx, {key, body})
        else
          false
        end
    end
  end

  # only deferred transactions
  def run_deferred_txs(conn, stmts, dets) do
    for {{type, _key}, [hash, account_id, validator_id, args, timestamp, size]} <-
          :ets.tab2list(:dtx) do
      %{modx: module, fun: fun} = Funcs.lookup(type)

      source = %{
        id: account_id,
        conn: conn,
        stmts: stmts,
        dets: dets,
        type: type,
        validator: validator_id,
        hash: hash,
        timestamp: timestamp,
        size: size
      }

      apply(module, fun, [source | args])
    end

    :ets.delete_all_objects(:dtx)
  end
end
