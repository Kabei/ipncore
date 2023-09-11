defmodule Ippan.EventHandler do
  alias Ippan.Events

  # @libsecp256k1 ExSecp256k1.Impl
  # @json Application.compile_env(:ipncore, :json)

  def handle!(
        conn,
        stmts,
        dets,
        hash,
        type,
        timestamp,
        account_id,
        validator,
        size,
        args,
        block_id
      ) do
    case Events.lookup(type) do
      %{deferred: false, mod: module, fun: fun} ->
        environment = %{
          conn: conn,
          block_id: block_id,
          stmts: stmts,
          dets: dets,
          id: account_id,
          type: type,
          validator: validator,
          hash: hash,
          timestamp: timestamp,
          size: size
        }

        apply(module, fun, [environment | args])

      # deferred transactions
      _deferred ->
        key = normalize_key(args, account_id)
        insert_deferred({{key, type}, hash, block_id, {args, validator.id, timestamp}})
    end
  end

  # only deferred transactions
  defmacro handle_post!(
             conn,
             stmts,
             dets,
             hash,
             type,
             account_id,
             validator_id,
             args,
             size,
             timestamp
           ) do
    quote bind_quoted: [
            conn: conn,
            stmts: stmts,
            dets: dets,
            hash: hash,
            type: type,
            account_id: account_id,
            validator_id: validator_id,
            args: args,
            size: size,
            timestamp: timestamp
          ] do
    end

    %{mod: module, fun: fun} = Events.lookup(type)

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

  # Dispute resolution in deferred transaction
  defp insert_deferred(
         {{_key, _type} = msg_key, hash, block_id, {_timestamp, _args, _validator_id}} = msg
       ) do
    case :ets.lookup(:dtx, msg_key) do
      [] ->
        :ets.insert(:dtx, msg)

      [{_msg_key, xhash, xblock_id, _rest}] ->
        if hash < xhash or (hash == xhash and block_id < xblock_id) do
          :ets.insert(:dtx, msg)
        else
          :error
        end
    end
  end

  defp normalize_key(nil, from), do: from
  defp normalize_key([], from), do: from
  defp normalize_key(args, _from), do: hd(args) |> to_string()
end
