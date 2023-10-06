defmodule Ippan.TxHandler do
  alias Ippan.{Funcs, Wallet}

  @compile {:inline, check_signature!: 4}

  @json Application.compile_env(:ipncore, :json)

  defmacrop get_and_check_wallet!(dets, cache, type, from, validator_id, args) do
    quote bind_quoted: [
            dets: dets,
            cache: cache,
            type: type,
            from: from,
            validator_id: validator_id,
            args: args
          ],
          location: :keep do
      case type do
        # check from variable
        0 ->
          {pk, vid} =
            DetsPlux.get_cache(dets, cache, from)

          if vid != validator_id do
            raise IppanRedirectError, "#{validator_id}"
          end

          pk

        # get first argument and not check (wallet subscribe)
        1 ->
          Fast64.decode64(hd(args))

        # check first argument
        2 ->
          key = hd(args)

          {pk, vid} =
            DetsPlux.get_tx(dets, cache, key)

          if vid != validator_id do
            raise IppanRedirectError, "#{vid}"
          end

          pk
      end
    end
  end

  @spec valid!(reference, map, binary, binary, binary, integer, integer, map) :: list()
  def valid!(conn, stmts, hash, msg, signature, size, validator_id, validator) do
    [type, timestamp, nonce, from | args] = @json.decode!(msg)

    %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification} = Funcs.lookup(type)

    wallet_dets = DetsPlux.get(:wallet)
    wallet_cache = DetsPlux.tx(:wallet)

    wallet_pk =
      get_and_check_wallet!(
        wallet_dets,
        wallet_cache,
        type_of_verification,
        from,
        validator_id,
        args
      )

    [sig_type, _] = String.split(from, "x", parts: 2)
    check_signature!(sig_type, signature, hash, wallet_pk)

    Wallet.update_nonce!(wallet_dets, :cache_nonce, from, nonce)

    source = %{
      conn: conn,
      balance: {DetsPlux.get(:balance), DetsPlux.tx(:cache_balance)},
      id: from,
      hash: hash,
      size: size,
      stmts: stmts,
      timestamp: timestamp,
      type: type,
      validator: validator,
      wallets: {wallet_dets, wallet_cache}
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
            nonce,
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
            nonce,
            [msg, signature],
            size
          ]
        ]
    end
  end

  @spec valid_from_file!(reference, map, tuple(), binary, binary, binary, integer, integer, map) ::
          list()
  def valid_from_file!(
        conn,
        stmts,
        {wallet_dets, wallet_cache} = wallets,
        hash,
        msg,
        signature,
        size,
        validator_id,
        validator
      ) do
    [type, timestamp, nonce, from | args] = @json.decode!(msg)

    %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification} = Funcs.lookup(type)

    wallet_pk =
      get_and_check_wallet!(
        wallet_dets,
        wallet_cache,
        type_of_verification,
        from,
        validator_id,
        args
      )

    [sig_type, _] = String.split(from, "x", parts: 2)
    check_signature!(sig_type, signature, hash, wallet_pk)

    Wallet.update_nonce!(wallet_dets, :nonce, from, nonce)

    source = %{
      conn: conn,
      balance: {DetsPlux.get(:balance), DetsPlux.tx(:cache_balance)},
      id: from,
      hash: hash,
      size: size,
      stmts: stmts,
      timestamp: timestamp,
      type: type,
      validator: validator,
      wallets: wallets
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
          nonce,
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
          nonce,
          size
        ]
    end
  end

  defmacro handle_regular(
             conn,
             stmts,
             balances,
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
            balance: balances,
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

      source = %{
        balance: balance,
        conn: conn,
        block_id: block_id,
        stmts: stmts,
        id: from,
        type: type,
        validator: validator,
        hash: hash,
        timestamp: timestamp,
        size: size
      }

      apply(module, fun, [source | args])
    end
  end

  # Dispute resolution in deferred transaction
  def insert_deferred(
        [hash, type, arg_key, account_id, args, timestamp, _nonce, size],
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
  def run_deferred_txs(conn, stmts, balances) do
    for {{type, _key}, [hash, account_id, validator_id, args, timestamp, _nonce, size]} <-
          :ets.tab2list(:dtx) do
      %{modx: module, fun: fun} = Funcs.lookup(type)

      source = %{
        id: account_id,
        conn: conn,
        stmts: stmts,
        balance: balances,
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
end
