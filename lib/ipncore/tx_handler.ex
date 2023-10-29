defmodule Ippan.TxHandler do
  alias Ippan.Funcs

  defmacro check_return! do
    quote location: :keep do
      case var!(return) do
        x when is_map(x) ->
          # Check balance
          dets = DetsPlux.get(:balance)
          cache = DetsPlux.tx(dets, :cache_balance)

          BalanceStore.multi_requires!(dets, cache, x)

        _ ->
          nil
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
      body = [var!(hash), var!(type), var!(from), var!(validator), var!(args), var!(size)]

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
      |> Enum.each(fn {{block_id, _ix},
                       [
                         hash,
                         type,
                         account_id,
                         validator,
                         args,
                         size
                       ]} ->
        %{modx: module, fun: fun} = Funcs.lookup(type)

        source = %{
          block: block_id,
          hash: hash,
          id: account_id,
          round: var!(round_id),
          size: size,
          type: type,
          validator: validator
        }

        apply(module, fun, [source | args])
      end)

      :ets.delete_all_objects(:dtx)
    end
  end
end
