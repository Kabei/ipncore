defmodule Ippan.TxHandler do
  alias Ippan.{Funcs, Wallet, TxHandler}

  defmacro get_public_key!(dets, tx, type, vid) do
    quote location: :keep, bind_quoted: [dets: dets, tx: tx, type: type, vid: vid] do
      case type do
        # check from variable
        0 ->
          {pk, v, sig_type} =
            DetsPlux.get_cache(dets, tx, var!(from))

          if vid != v do
            raise IppanRedirectError, "#{v}"
          end

          {pk, sig_type}

        # get from argument and not check (wallet.new)
        1 ->
          [pk, _, sig_type | _rest] = var!(args)
          {Fast64.decode64(pk), sig_type}

        # check first argument
        2 ->
          key = hd(var!(args))

          {pk, v, sig_type} =
            DetsPlux.get_tx(dets, tx, key)

          if v != vid do
            raise IppanRedirectError, "#{v}"
          end

          {pk, sig_type}

        # get from variable not redirect
        3 ->
          {pk, _v, sig_type} =
            DetsPlux.get_cache(dets, tx, var!(from))

          {pk, sig_type}
      end
    end
  end

  # check signature by type
  defmacro check_signature!(sig_type, pk) do
    quote location: :keep,
          bind_quoted: [pk: pk, sig_type: sig_type] do
      case sig_type do
        0 ->
          # verify ed25519 signature
          if Cafezinho.Impl.verify(var!(signature), var!(hash), pk) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        1 ->
          # verify secp256k1 signature
          if ExSecp256k1.Impl.verify(var!(hash), var!(signature), pk) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        2 ->
          # verify falcon-512 signature
          if Falcon.verify(var!(hash), var!(signature), pk) != :ok,
            do: raise(IppanError, "Invalid signature verify")

        _ ->
          raise(IppanError, "Signature type: #{sig_type} is not supported")
      end
    end
  end

  # @spec valid!(reference, map, binary, binary, binary, integer, integer, map) :: list()
  # (conn, stmts, hash, msg, signature, size, validator_id, validator)
  defmacro decode! do
    quote location: :keep do
      %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification, key: key_unique} =
        Funcs.lookup(var!(type))

      wallet_dets = DetsPlux.get(:wallet)
      wallet_cache = DetsPlux.tx(wallet_dets, :cache_wallet)

      {wallet_pk, sig_type} =
        TxHandler.get_public_key!(wallet_dets, wallet_cache, type_of_verification, var!(vid))

      TxHandler.check_signature!(sig_type, wallet_pk)

      # Check nonce
      nonce_dets = DetsPlux.get(:nonce)
      cache_nonce_tx = DetsPlux.tx(nonce_dets, :cache_nonce)
      Wallet.gte_nonce!(nonce_dets, cache_nonce_tx, var!(from), var!(nonce))

      source = %{
        id: var!(from),
        hash: var!(hash),
        nonce: var!(nonce),
        size: var!(size),
        type: var!(type),
        validator: var!(validator)
      }

      return = apply(mod, fun, [source | var!(args)])

      case return do
        :error ->
          raise IppanError, "Invalid returned value"

        {:error, message} ->
          raise IppanError, message

        _ ->
          case deferred do
            false ->
              [
                deferred,
                [
                  var!(hash),
                  var!(type),
                  var!(from),
                  var!(nonce),
                  var!(args),
                  [var!(body), var!(signature)],
                  var!(size)
                ],
                return
              ]

            _true ->
              key =
                case key_unique do
                  1 ->
                    var!(from)

                  _ ->
                    hd(var!(args)) |> to_string()
                end

              [
                deferred,
                [
                  var!(hash),
                  var!(type),
                  key,
                  var!(from),
                  var!(nonce),
                  var!(args),
                  [var!(body), var!(signature)],
                  var!(size)
                ],
                return
              ]
          end
      end
    end
  end

  defmacro decode_from_file! do
    quote location: :keep do
      %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification, key: key_unique} =
        Funcs.lookup(var!(type))

      {wallet_pk, sig_type} =
        TxHandler.get_public_key!(
          var!(wallet_dets),
          var!(wallet_tx),
          type_of_verification,
          var!(creator_id)
        )

      TxHandler.check_signature!(sig_type, wallet_pk)

      Wallet.gte_nonce!(var!(nonce_dets), var!(nonce_tx), var!(from), var!(nonce))

      source = %{
        id: var!(from),
        hash: var!(hash),
        nonce: var!(nonce),
        size: var!(size),
        type: var!(type),
        validator: var!(validator)
      }

      apply(mod, fun, [source | var!(args)])

      case deferred do
        false ->
          [
            var!(hash),
            var!(type),
            var!(from),
            var!(nonce),
            var!(args),
            var!(signature),
            var!(size)
          ]

        _true ->
          key =
            case key_unique do
              1 ->
                var!(from)

              _ ->
                hd(var!(args)) |> to_string()
            end

          [
            var!(hash),
            var!(type),
            key,
            var!(from),
            var!(nonce),
            var!(args),
            var!(signature),
            var!(size)
          ]
      end
    end
  end

  @spec regular() :: any | :error
  defmacro regular do
    quote location: :keep do
      %{fun: fun, modx: module} = Funcs.lookup(var!(type))

      source = %{
        block: var!(block_id),
        hash: var!(hash),
        id: var!(from),
        nonce: var!(nonce),
        round: var!(round_id),
        size: var!(size),
        type: var!(type),
        validator: var!(validator)
      }

      apply(module, fun, [source | var!(args)])
    end
  end

  # Dispute resolution in deferred transaction
  defmacro insert_deferred(table, tmp_table) do
    quote bind_quoted: [table: table, tmp: tmp_table], location: :keep do
      key = {var!(type), var!(arg_key)}
      order = {var!(block_id), var!(ix)}

      body = [
        var!(hash),
        var!(type),
        var!(from),
        var!(validator),
        var!(nonce),
        var!(args),
        var!(size)
      ]

      case :ets.lookup(tmp, key) do
        [] ->
          :ets.insert(tmp, {key, var!(hash), var!(block_id)})
          :ets.insert(table, {order, body})

        [{_msg_key, xhash, xblock_id}] ->
          if var!(hash) < xhash or (var!(hash) == xhash and var!(block_id) < xblock_id) do
            :ets.insert(tmp, {key, var!(hash), var!(block_id)})
            :ets.insert(table, {order, body})
          else
            false
          end
      end
    end
  end

  # only deferred transactions
  defmacro run_deferred_txs do
    quote location: :keep do
      :ets.tab2list(:dtx)
      |> Enum.each(fn
        {{block_id, _ix},
         [
           hash,
           type,
           account_id,
           validator,
           nonce,
           args,
           size
         ]} ->
          %{modx: module, fun: fun} = Funcs.lookup(type)

          source = %{
            block: block_id,
            hash: hash,
            id: account_id,
            nonce: nonce,
            round: var!(round_id),
            size: size,
            type: type,
            validator: validator
          }

          apply(module, fun, [source | args])

        {_block_and_tx_hash, fun} ->
          fun.()
      end)

      :ets.delete_all_objects(:dtx)
    end
  end
end
