defmodule Ippan.TxHandler do
  alias Ippan.Funcs

  defmacro check_balance! do
    quote location: :keep do
      if is_map(var!(return)) do
        # Check balance
        dets = DetsPlux.get(:balance)
        cache = DetsPlux.tx(:cache_balance)

        Enum.each(var!(return), fn {key, value} ->
          BalanceStore.requires!(dets, cache, key, value)
        end)
      end
    end
  end

  defmacro handle_regular(
             conn,
             stmts,
             balances,
             wallets,
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
            wallets: wallets,
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
        size: size,
        wallets: wallets
      }

      apply(module, fun, [source | args])
    end
  end

  # Dispute resolution in deferred transaction
  @spec insert_deferred(list(), pos_integer(), pos_integer()) :: boolean
  def insert_deferred(
        [hash, type, arg_key, account_id, args, timestamp, _nonce, size],
        validator_id,
        block_id
      ) do
    key = {type, arg_key}
    body = [hash, account_id, validator_id, args, timestamp, size, block_id]

    case :ets.lookup(:dtx, key) do
      [] ->
        :ets.insert(:dtx, {key, body})

      [{_msg_key, [xhash | _rest] = xbody}] ->
        xblock_id = List.last(xbody)

        if hash < xhash or (hash == xhash and block_id < xblock_id) do
          :ets.insert(:dtx, {key, body})
        else
          false
        end
    end
  end

  # only deferred transactions
  @spec run_deferred_txs(any, any, any, any, any) :: true
  def run_deferred_txs(conn, stmts, balance_pid, balance_tx, wallets) do
    IO.puts("txs deferred")
    IO.inspect(:ets.tab2list(:dtx))

    for m = {{type, _key}, [hash, account_id, validator_id, args, timestamp, size, block_id]} <-
          :ets.tab2list(:dtx) do
      %{modx: module, fun: fun} = Funcs.lookup(type)
      IO.inspect(m)

      source = %{
        id: account_id,
        block_id: block_id,
        conn: conn,
        stmts: stmts,
        balance: {balance_pid, balance_tx},
        type: type,
        validator: validator_id,
        hash: hash,
        timestamp: timestamp,
        size: size,
        wallets: wallets
      }

      apply(module, fun, [source | args])
    end

    :ets.delete_all_objects(:dtx)
  end
end
