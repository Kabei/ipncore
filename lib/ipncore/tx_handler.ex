defmodule Ippan.TxHandler do
  alias __MODULE__
  alias Ippan.{Funcs, Account, TxHandler, Validator}
  require Sqlite
  require Validator

  def data_refs do
    :persistent_term.get(:data_refs)
  end

  def check_nonce!(dets, tx, from, nonce) do
    count = DetsPlux.get_cache(dets, tx, from, 0)

    if count + 1 == nonce do
      raise IppanError, "Invalid nonce"
    end
  end

  defmacro get_index(type_of_index, type, from, nonce, args) do
    quote location: :keep,
          bind_quoted: [
            type_index: type_of_index,
            from: from,
            nonce: nonce,
            type: type,
            args: args
          ] do
      case type_index do
        0 ->
          from

        1 ->
          {type, from}

        2 ->
          {from, nonce}

        3 ->
          type

        {:type, :first} ->
          {type, hd(args)}

        {:type, :first, 2} ->
          [a, b | _] = args
          {type, a, b}

        {:arg, :first} ->
          hd(args)

        {:arg, :third} ->
          [_, _, x | _] = args
          x

        {:arg, :last} ->
          :lists.last(args)

        {:arg, :first, 2} ->
          [a, b | _] = args
          {a, b}

        {:arg, :first, 3} ->
          [a, b, c | _] = args
          {a, b, c}

        {:from, :first} ->
          {from, hd(args)}
      end
    end
  end

  defmacro get_account!(dets, tx, db_ref, type_of_check, from, vid, args) do
    quote location: :keep,
          bind_quoted: [
            args: args,
            db_ref: db_ref,
            from: from,
            dets: dets,
            tx: tx,
            type: type_of_check,
            vid: vid
          ] do
      case type do
        # get from FROM variable not check its validator
        0 ->
          DetsPlux.get_cache(dets, tx, from)

        # get data from FROM and check its validator
        1 ->
          {pk, sig_type, %{"vid" => v} = account_data} =
            DetsPlux.get_cache(dets, tx, from)

          if vid != v do
            raise IppanRedirectError, "#{v}"
          end

          {pk, sig_type, account_data}

        # get data from FROM and check its validator if not check if validator exists
        2 ->
          {pk, sig_type, account_data} =
            DetsPlux.get_cache(dets, tx, from)

          v = Map.get(account_data, "vid")

          if vid != v and Validator.exists?(v) do
            raise IppanRedirectError, "#{v}"
          end

          {pk, sig_type, account_data}

        # get data from argument position (account.new)
        {:pk, :first} ->
          [pk, sig_type | _] = args
          {Fast64.decode64(pk), sig_type, nil}

        # check validator from argument position and get data from FROM variable
        {:check, pos} ->
          from =
            case pos do
              0 ->
                hd(args)

              1 ->
                [_, x | _] = args
                x

              2 ->
                [_, _, x | _] = args
                x
            end

          result = {_, _sig_type, %{"vid" => v}} = DetsPlux.get_cache(dets, tx, from)

          if vid != v do
            raise IppanRedirectError, "#{v}"
          end

          DetsPlux.get_cache(dets, tx, from)
      end
    end
  end

  @error_signature "Invalid signature verify"
  # check signature by type
  defmacro check_signature!(sig_type, hash, pk, signature) do
    quote location: :keep,
          bind_quoted: [hash: hash, pk: pk, sig_type: sig_type, sig: signature] do
      case sig_type do
        0 ->
          # verify ed25519 signature
          if Cafezinho.Impl.verify(sig, hash, pk) != :ok,
            do: raise(IppanHighError, @error_signature)

        1 ->
          # verify secp256k1 signature
          if ExSecp256k1.Impl.verify(hash, sig, pk) != :ok,
            do: raise(IppanHighError, @error_signature)

        2 ->
          # verify falcon-512 signature
          if Falcon.verify(hash, sig, pk) != :ok,
            do: raise(IppanHighError, @error_signature)

        _ ->
          raise(IppanHighError, "Signature type: #{sig_type} is not supported")
      end
    end
  end

  @spec valid?(map()) :: true | false | no_return()
  def valid?(%{
        type:
          %{
            id: type_id,
            index: type_of_index,
            fun: fun,
            mod: mod,
            origin: type_of_origin
          } = type,
        from: from,
        nonce: nonce,
        args: args,
        hash: hash,
        size: size,
        validator: %{vid: vid, fa: vfa, fb: vfb} = validator,
        ets: ets,
        refs:
          %{
            dets: %{nonce: nonce_dets, wallet: wallet_dets},
            tx: %{nonce: nonce_tx, wallet: wallet_tx},
            db: db_ref
          } = refs,
        sig: signature
      }) do
    key =
      get_index(type_of_index, type, from, nonce, args)

    if :ets.member(ets, key) do
      raise IppanHighError, "Already exists transaction"
    end

    {pk, sig_type, %{"fa" => fa, "fb" => fb} = account_data} =
      get_account!(wallet_dets, wallet_tx, db_ref, type_of_origin, from, vid, args)

    if account_data != nil and
         type_of_origin != 2 and
         (fb != vfb or fa != vfa),
       do: raise(IppanError, "Invalid fees")

    # check signature by type of signature
    check_signature!(sig_type, pk, hash, signature)

    # Check nonce
    check_nonce!(nonce_dets, nonce_tx, from, nonce)

    source = %{
      id: from,
      refs: refs,
      hash: hash,
      data: account_data,
      nonce: nonce,
      size: size,
      type: type,
      validator: validator
    }

    return = apply(mod, fun, [source | args])

    case return do
      :error ->
        raise IppanError, "Invalid returned value"

      {:error, message} ->
        raise IppanError, message

      _ ->
        result = {hash, type_id, from, nonce, return, size, signature}
        :ets.insert(ets, {key, result})
    end
  end

  # Dispute resolution in deferred transaction
  def insert_deferred(
        table,
        {key, body = {hash, type_id, _from, _nonce, _args, _size, _signature}},
        block_id
      ) do
    %{unique: unique} = Funcs.lookup(type_id)

    cond do
      unique == false ->
        :ets.insert(table, {key, body})

      true ->
        deferred_key = {type_id, key}

        case :ets.lookup(table, deferred_key) do
          [] ->
            :ets.insert(table, {deferred_key, body, block_id})

          [{_def_key, body, xblock_id}] ->
            xhash = :erlang.element(1, body)

            if hash < xhash or (hash == xhash and block_id < xblock_id) do
              :ets.insert(table, {deferred_key, body, block_id})
            end
        end
    end
  end

  # defmacro decode_from_file! do
  #   quote location: :keep do
  #     %{deferred: deferred, mod: mod, fun: fun, check: type_of_verification, key: key_unique} =
  #       Funcs.lookup(var!(type))

  #     {wallet_pk, sig_type, account_data} =
  #       TxHandler.get_public_key!(
  #         var!(wallet_dets),
  #         var!(wallet_tx),
  #         type_of_verification,
  #         var!(creator_id)
  #       )

  #     if account_data != nil do
  #       %{"fa" => fa, "fb" => fb} = account_data
  #       %{fa: vfa, fb: vfb} = var!(validator)

  #       if type_of_verification != 2 and (fb != vfb or fa != vfa),
  #         do: raise(IppanError, "Invalid fees")
  #     end

  #     TxHandler.check_signature!(sig_type, wallet_pk)

  #     Account.update_nonce!(var!(nonce_dets), var!(nonce_tx), var!(from), var!(nonce))

  #     source = %{
  #       id: var!(from),
  #       dets: var!(dets),
  #       hash: var!(hash),
  #       map: account_data,
  #       nonce: var!(nonce),
  #       size: var!(size),
  #       type: var!(type),
  #       validator: var!(validator)
  #     }

  #     return = apply(mod, fun, [source | var!(args)])

  #     case return do
  #       :error ->
  #         [
  #           "err",
  #           var!(hash),
  #           var!(type),
  #           var!(from),
  #           var!(nonce),
  #           var!(args),
  #           var!(signature),
  #           var!(size)
  #         ]

  #       _ ->
  #         case deferred do
  #           false ->
  #             [
  #               var!(hash),
  #               var!(type),
  #               var!(from),
  #               var!(nonce),
  #               var!(args),
  #               var!(signature),
  #               var!(size)
  #             ]

  #           _true ->
  #             key =
  #               case key_unique do
  #                 1 ->
  #                   var!(from)

  #                 2 ->
  #                   hd(var!(args)) |> to_string()
  #               end

  #             [
  #               var!(hash),
  #               var!(type),
  #               key,
  #               var!(from),
  #               var!(nonce),
  #               var!(args),
  #               var!(signature),
  #               var!(size)
  #             ]
  #         end
  #     end
  #   end
  # end

  # @spec regular() :: any | :error
  # defmacro regular do
  #   quote location: :keep do
  #     %{fun: fun, modx: module} = Funcs.lookup(var!(type))

  #     source = %{
  #       block: var!(block_id),
  #       hash: var!(hash),
  #       id: var!(from),
  #       nonce: var!(nonce),
  #       round: var!(round_id),
  #       size: var!(size),
  #       type: var!(type),
  #       validator: var!(validator)
  #     }

  #     apply(module, fun, [source | var!(args)])
  #   end
  # end

  # only deferred transactions
  # defmacro run_deferred_txs do
  #   quote location: :keep do
  #     :ets.tab2list(:dtx)
  #     |> Enum.each(fn
  #       {{block_id, _ix},
  #        [
  #          hash,
  #          type,
  #          account_id,
  #          validator,
  #          nonce,
  #          args,
  #          size
  #        ] = body} ->
  #         %{modx: module, fun: fun} = Funcs.lookup(type)

  #         source = %{
  #           block: block_id,
  #           hash: hash,
  #           id: account_id,
  #           nonce: nonce,
  #           round: var!(round_id),
  #           size: size,
  #           type: type,
  #           validator: validator
  #         }

  #         apply(module, fun, [source | args])

  #       {_block_and_tx_hash, fun} ->
  #         fun.()
  #     end)

  #     :ets.delete_all_objects(:dtx)
  #   end
  # end

  @app Mix.Project.config()[:app]
  @max_shards Application.compile_env(@app, :max_shards, 1000)

  @spec get_flag(type_flag :: term(), from :: binary(), args :: list(), shard :: integer()) ::
          integer()
  def get_flag(0, from, _args, shard) do
    rem(:erlang.phash2(from, @max_shards), shard)
  end

  def get_flag({:arg, 0}, _from, [value | _], shard) do
    rem(:erlang.phash2(value, @max_shards), shard)
  end

  def get_flag({:arg, 1}, _from, [_, value | _], shard) do
    rem(:erlang.phash2(value, @max_shards), shard)
  end

  def get_flag({:arg, :first, 2}, _from, [a, b | _], shard) do
    rem(:erlang.phash2({a, b}, @max_shards), shard)
  end

  def check_flag(flag, from, args) do
    shard = :persistent_term.get(:shard)

    get_flag(flag, from, args, shard) == shard - 1
  end

  def check_flag!(flag, from, args) do
    if check_flag(flag, from, args), do: raise(IppanError, "Invalid shard")
  end
end
