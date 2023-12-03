defmodule BalanceStore do
  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)

  defmacro requires!(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      DetsPlux.get_cache(dets, tx, key, {0, %{}})

      if DetsPlux.update_counter(tx, key, {2, -value}) < 0 do
        DetsPlux.update_counter(tx, key, {2, value})
        raise IppanError, "Insufficient balance"
      end
    end
  end

  defmacro multi_requires!(dets, tx, key_value_list) do
    quote bind_quoted: [dets: dets, tx: tx, list: key_value_list], location: :keep do
      Enum.reduce(list, [], fn kv = {key, value}, acc ->
        DetsPlux.get_cache(dets, tx, key, {0, %{}})

        if DetsPlux.update_counter(tx, key, {2, -value}) < 0 do
          DetsPlux.update_counter(tx, key, {2, value})

          Enum.each(acc, fn {key, value} ->
            DetsPlux.update_counter(tx, key, {2, value})
          end)

          raise IppanError, "Insufficient balance"
        else
          [kv | acc]
        end
      end)
    end
  end

  defmacro send(amount) do
    quote bind_quoted: [amount: amount], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))
      DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, %{}})
      DetsPlux.get_cache(var!(dets), var!(tx), to_balance_key, {0, %{}})

      DetsPlux.update_counter(var!(tx), balance_key, {2, -amount})
      DetsPlux.update_counter(var!(tx), to_balance_key, {2, amount})
    end
  end

  defmacro refund(from, old_sender, token, amount) do
    quote bind_quoted: [from: from, to: old_sender, token: token, amount: amount],
          location: :keep do
      balance_key = DetsPlux.tuple(from, token)
      to_balance_key = DetsPlux.tuple(to, token)
      DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, %{}})
      DetsPlux.get_cache(var!(dets), var!(tx), to_balance_key, {0, %{}})

      DetsPlux.update_counter(var!(tx), balance_key, {2, -amount})
      DetsPlux.update_counter(var!(tx), to_balance_key, {2, amount})
    end
  end

  defmacro fees(total_fees, fees, reserve) do
    quote bind_quoted: [total_fees: total_fees, fees: fees, reserve: reserve, token: @token],
          location: :keep do
      balance_key = DetsPlux.tuple(var!(from), token)
      validator_key = DetsPlux.tuple(var!(vOwner), token)
      DetsPlux.get_cache(var!(dets), var!(tx), balance_key, {0, %{}})
      DetsPlux.get_cache(var!(dets), var!(tx), validator_key, {0, %{}})

      DetsPlux.update_counter(var!(tx), balance_key, {2, -total_fees})
      DetsPlux.update_counter(var!(tx), validator_key, {2, fees})
      BalanceStore.reserve(reserve)
    end
  end

  defmacro burn(account, token, amount) do
    quote bind_quoted: [account: account, token: token, amount: amount], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})
      DetsPlux.update_counter(var!(tx), key, {2, -amount})
      TokenSupply.subtract(var!(supply), amount)
    end
  end

  defmacro reserve(amount) do
    quote bind_quoted: [token: @token, amount: amount], location: :keep do
      if amount > 0 do
        supply = TokenSupply.new("jackpot")
        TokenSupply.add(supply, amount)
      end
    end
  end

  defmacro fees_burn(supply, amount) do
    quote bind_quoted: [supply: supply, token: @token, amount: amount], location: :keep do
      TokenSupply.subtract(supply, amount)
    end
  end

  defmacro coinbase(account, token, value) do
    quote bind_quoted: [account: account, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})
      DetsPlux.update_counter(var!(tx), key, {2, value})
    end
  end

  defmacro reload(account, token, value) do
    quote bind_quoted: [account: account, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(account, token)
      DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})
      DetsPlux.update_counter(var!(tx), key, {2, value})
    end
  end

  defmacro lock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, map} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})

      if balance >= value do
        lock = Map.get(map, "lock", 0)
        map = Map.put(map, "lock", lock + value)
        DetsPlux.update_counter(var!(tx), key, [{2, -value}])
        DetsPlux.update_element(var!(tx), key, 3, map)
      else
        :error
      end
    end
  end

  defmacro unlock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, map} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})
      lock = Map.get(map, "lock", 0)

      if lock >= value do
        map = Map.put(map, "lock", lock - value)
        DetsPlux.update_counter(var!(tx), key, [{2, value}])
        DetsPlux.update_element(var!(tx), key, 3, map)
      else
        :error
      end
    end
  end

  defmacro pay_fee(from, to, total_fees) do
    quote bind_quoted: [from: from, to: to, token: @token, total_fees: total_fees],
          location: :keep do
      if to != from do
        key = DetsPlux.tuple(from, token)
        {balance, _map} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})

        result = balance - total_fees

        if result >= 0 do
          to_key = DetsPlux.tuple(to, token)
          DetsPlux.get_cache(var!(dets), var!(tx), to_key, {0, %{}})

          reserve = Ippan.Utils.calc_reserve(total_fees)
          fees = total_fees - reserve

          DetsPlux.update_counter(var!(tx), key, {2, -total_fees})
          DetsPlux.update_counter(var!(tx), to_key, {2, fees})
          BalanceStore.reserve(reserve)
        else
          :error
        end
      else
        BalanceStore.pay_burn(from, Ippan.Utils.calc_reserve(total_fees))
      end
    end
  end

  defmacro pay_burn(from, value) do
    quote bind_quoted: [from: from, token: @token, value: value],
          location: :keep do
      key = DetsPlux.tuple(from, token)
      {balance, _map} = DetsPlux.get_cache(var!(dets), var!(tx), key, {0, %{}})

      result = balance - value

      if result >= 0 do
        DetsPlux.update_counter(var!(tx), key, {2, -value})

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
      DetsPlux.get_cache(dets, tx, key, {0, %{}})
      DetsPlux.update_counter(tx, key, {2, value})
    end
  end
end
