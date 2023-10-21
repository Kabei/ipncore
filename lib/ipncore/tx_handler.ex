defmodule Ippan.TxHandler do
  alias Ippan.Funcs

  defmacro check_return! do
    quote location: :keep do
      case var!(return) do
        x when is_map(x) ->
          # Check balance
          dets = DetsPlux.get(:balance)
          cache = DetsPlux.tx(:cache_balance)

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
  @spec insert_deferred() :: boolean()
  defmacro insert_deferred do
    quote location: :keep do
      key = {var!(type), var!(arg_key)}
      body = [var!(hash), var!(from), var!(validator), var!(args), var!(size), var!(block_id)]

      case :ets.lookup(:dtx, key) do
        [] ->
          :ets.insert(:dtx, {key, body})

        [{_msg_key, [xhash | _rest] = xbody}] ->
          xblock_id = List.last(xbody)

          if var!(hash) < xhash or (var!(hash) == xhash and var!(block_id) < xblock_id) do
            :ets.insert(:dtx, {key, body})
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
      |> Enum.each(fn {{type, _key},
                       [
                         hash,
                         account_id,
                         validator,
                         args,
                         size,
                         block_id
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
