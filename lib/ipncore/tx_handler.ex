defmodule Ippan.TxHandler do
  alias Ippan.Funcs

  # @libsecp256k1 ExSecp256k1.Impl
  # @json Application.compile_env(:ipncore, :json)

  # def handle!(
  #       conn,
  #       stmts,
  #       dets,
  #       hash,
  #       type,
  #       timestamp,
  #       account_id,
  #       validator,
  #       size,
  #       args,
  #       block_id
  #     ) do
  #   case Funcs.lookup(type) do
  #     %{deferred: false, mod: module, fun: fun} ->
  #       environment = %{
  #         conn: conn,
  #         block_id: block_id,
  #         stmts: stmts,
  #         dets: dets,
  #         id: account_id,
  #         type: type,
  #         validator: validator,
  #         hash: hash,
  #         timestamp: timestamp,
  #         size: size
  #       }

  #       apply(module, fun, [environment | args])

  #     # deferred transactions
  #     _deferred ->
  #       key = normalize_key(args, account_id)
  #       insert_deferred({{key, type}, hash, block_id, {args, validator.id, timestamp}})
  #   end
  # end

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
          ] do
      %{fun: fun, mod: module} = Funcs.lookup(type)

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
    for {{_key, type}, [hash, account_id, validator_id, args, timestamp, size]} <-
          :ets.tab2list(:dtx) do
      %{mod: module, fun: fun} = Funcs.lookup(type)

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
