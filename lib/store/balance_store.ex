defmodule BalanceStore do
  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)

  defmacro requires!(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock} = DetsPlux.get_cache(dets, tx, key, {0, 0})

      case balance >= value do
        true ->
          DetsPlux.update_counter(tx, key, {2, -value})

        false ->
          raise IppanError, "Insufficient balance"
      end
    end
  end

  defmacro multi_requires!(dets, tx, key_value_list) do
    quote bind_quoted: [dets: dets, tx: tx, list: key_value_list], location: :keep do
      Enum.map(list, fn {key, value} ->
        {balance, lock} = DetsPlux.get_cache(dets, tx, key, {0, 0})

        case balance >= value do
          true ->
            {key, value, balance, lock}

          false ->
            raise IppanError, "Insufficient balance"
        end
      end)
      |> Enum.each(fn {key, value, balance, lock} ->
        DetsPlux.update_counter(tx, key, {2, -value})
      end)
    end
  end

  defmacro send(amount) do
    quote bind_quoted: [amount: amount], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))
      DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, 0})
      DetsPlux.get_cache(var!(dets), var!(tx), to_balance_key, {0, 0})

      DetsPlux.update_counter(var!(tx), balance_key, {2, -amount})
      DetsPlux.update_counter(var!(tx), to_balance_key, {2, amount})
    end
  end

  defmacro refund(amount) do
    quote bind_quoted: [amount: amount], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))
      DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, 0})
      DetsPlux.get_cache(var!(dets), var!(tx), to_balance_key, {0, 0})

      DetsPlux.update_counter(var!(tx), balance_key, {2, -amount})
      DetsPlux.update_counter(var!(tx), to_balance_key, {2, amount})
    end
  end

  defmacro fees(fees, remove) do
    quote bind_quoted: [fees: fees, remove: remove, token: @token], location: :keep do
      if fees > 0 do
        balance_key = DetsPlux.tuple(var!(from), var!(token_id))
        validator_key = DetsPlux.tuple(var!(vOwner), var!(token_id))
        DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, 0})
        DetsPlux.get_cache(var!(dets), var!(tx), validator_key, {0, 0})

        DetsPlux.update_counter(var!(tx), balance_key, {2, -fees - remove})
        DetsPlux.update_counter(var!(tx), validator_key, {2, fees})
      else
        BalanceStore.delete(var!(from), var!(token_id), var!(remove))
      end
    end
  end

  defmacro coinbase(account, token, value) do
    quote bind_quoted: [account: account, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})
      DetsPlux.update_counter(var!(tx), key, {2, value})
    end
  end

  defmacro delete(account, token, amount) do
    quote bind_quoted: [account: account, token: token, amount: amount], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})
      DetsPlux.update_counter(var!(tx), key, {2, -amount})
      TokenSupply.subtract(var!(supply), amount)
    end
  end

  defmacro burn(account, token, amount) do
    quote bind_quoted: [account: account, token: token, amount: amount], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})
      DetsPlux.update_counter(var!(tx), key, {2, -amount})
      TokenSupply.subtract(var!(supply), amount)
    end
  end

  defmacro lock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, lock} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})

      if balance >= value do
        DetsPlux.update_counter(var!(tx), key, [{2, -value}, {3, value}])
      else
        :error
      end
    end
  end

  defmacro unlock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, lock} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})

      if balance >= value do
        DetsPlux.update_counter(var!(tx), key, [{2, value}, {3, -value}])
      else
        :error
      end
    end
  end

  defmacro pay_fee(from, to, value) do
    quote bind_quoted: [from: from, to: to, token: @token, value: value],
          location: :keep do
      if to != from do
        key = DetsPlux.tuple(from, token)
        {balance, _lock} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})

        result = balance - value

        if result >= 0 do
          to_key = DetsPlux.tuple(to, token)
          DetsPlux.get_cache(var!(dets), var!(tx), to_key, {0, 0})

          DetsPlux.update_counter(var!(tx), key, {2, -value})
          DetsPlux.update_counter(var!(tx), to_key, {2, value})
        else
          :error
        end
      else
        BalanceStore.pay_burn(from, value)
      end
    end
  end

  defmacro pay_burn(from, value) do
    quote bind_quoted: [from: from, token: @token, value: value],
          location: :keep do
      key = DetsPlux.tuple(from, token)
      {balance, _lock} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, 0})

      result = balance - value

      if result >= 0 do
        DetsPlux.update_counter(var!(tx), key, {2, -result})

        supply = TokenSupply.new(token)
        TokenSupply.subtract(supply, value)
      else
        :error
      end
    end
  end

  defmacro income(dets, tx, account, token, value) do
    quote bind_quoted: [dets: dets, tx: tx, account: account, token: token, value: value],
          location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(dets, tx, key, {0, 0})
      DetsPlux.update_counter(tx, key, {2, value})
    end
  end
end
