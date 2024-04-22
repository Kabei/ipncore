defmodule Ippan.TxHandler do
  alias Ippan.Validator
  require Sqlite
  require Validator

  @compile {:inline, [get_part: 4, data_refs: 0]}

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
            raise IppanRedirectError, v
          end

          {pk, sig_type, account_data}

        # get data from FROM and check its validator if not check if validator exists
        2 ->
          {pk, sig_type, account_data} =
            DetsPlux.get_cache(dets, tx, from)

          v = Map.get(account_data, "vid")

          if vid != v and Validator.exists?(v) do
            raise IppanRedirectError, v
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
            raise IppanRedirectError, v
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

  @spec valid?(map()) :: {binary, tuple()} | no_return()
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
        {key, result}
    end
  end

  @spec get_part(type_flag :: term(), from :: binary(), args :: list(), partition :: integer()) ::
          integer()
  def get_part(0, from, _args, partition) do
    rem(:erlang.phash2(from), partition)
  end

  def get_part({:arg, 0}, _from, [value | _], partition) do
    rem(:erlang.phash2(value), partition)
  end

  def get_part({:arg, 1}, _from, [_, value | _], partition) do
    rem(:erlang.phash2(value), partition)
  end

  def get_part({:arg, :first, 2}, _from, [a, b | _], partition) do
    rem(:erlang.phash2({a, b}), partition)
  end

  def check_part(flag, from, args) do
    partition = :persistent_term.get(:partition)

    get_part(flag, from, args, partition) == partition - 1
  end

  def check_part!(flag, from, args) do
    partition = :persistent_term.get(:partition)

    if get_part(flag, from, args, partition) == partition - 1 do
      raise(IppanError, "Invalid partition")
    end
  end
end
