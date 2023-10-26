defmodule BalanceStore do
  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)

  defmacro requires!(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      case balance >= value do
        true ->
          DetsPlux.put(tx, key, {balance - value, lock})

        false ->
          raise IppanError, "Insufficient balance"
      end
    end
  end

  defmacro multi_requires!(dets, tx, key_value_list) do
    quote bind_quoted: [dets: dets, tx: tx, list: key_value_list], location: :keep do
      Enum.map(list, fn {key, value} ->
        {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

        case balance >= value do
          true ->
            {key, value, balance, lock}

          false ->
            raise IppanError, "Insufficient balance"
        end
      end)
      |> Enum.each(fn {key, value, balance, lock} ->
        DetsPlux.put(tx, key, {balance - value, lock})
      end)
    end
  end

  defmacro send(amount) do
    quote bind_quoted: [amount: amount], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))

      {balance, lock1} = DetsPlux.get_tx(var!(dets), var!(tx), balance_key, {0, 0})

      {balance2, lock2} =
        DetsPlux.get_tx(var!(dets), var!(tx), to_balance_key, {0, 0})

      DetsPlux.put(var!(tx), balance_key, {balance - amount, lock1})
      DetsPlux.put(var!(tx), to_balance_key, {balance2 + amount, lock2})
    end
  end

  defmacro send(amount, remove) do
    quote bind_quoted: [
            amount: amount,
            remove: remove,
            token: @token
          ],
          location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))

      {balance, lock1} = DetsPlux.get_tx(var!(dets), var!(tx), balance_key, {0, 0})

      {balance2, lock2} =
        DetsPlux.get_tx(var!(dets), var!(tx), to_balance_key, {0, 0})

      DetsPlux.put(var!(tx), balance_key, {balance - amount - remove, lock1})
      DetsPlux.put(var!(tx), to_balance_key, {balance2 + amount, lock2})
      TokenSupply.subtract(var!(supply), remove)
    end
  end

  defmacro refund(amount) do
    quote bind_quoted: [amount: amount], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))

      {balance, lock1} = DetsPlux.get_tx(var!(dets), var!(tx), balance_key, {0, 0})

      {balance2, lock2} =
        DetsPlux.get_tx(var!(dets), var!(tx), to_balance_key, {0, 0})

      DetsPlux.put(var!(tx), balance_key, {balance - amount, lock1})
      DetsPlux.put(var!(tx), to_balance_key, {balance2 + amount, lock2})
    end
  end

  defmacro send(amount, fees, remove) do
    quote bind_quoted: [
            amount: amount,
            fees: fees,
            remove: remove
          ],
          location: :keep do
      balance_key = DetsPlux.tuple(var!(from), var!(token_id))
      to_balance_key = DetsPlux.tuple(var!(to), var!(token_id))
      validator_balance_key = DetsPlux.tuple(var!(vOwner), var!(token_id))

      {balance, lock1} = DetsPlux.get_tx(var!(dets), var!(tx), balance_key, {0, 0})

      {balance2, lock2} =
        DetsPlux.get_tx(var!(dets), var!(tx), to_balance_key, {0, 0})

      {balance3, lock3} =
        DetsPlux.get_tx(var!(dets), var!(tx), validator_balance_key, {0, 0})

      DetsPlux.put(var!(tx), balance_key, {balance - amount - fees - remove, lock1})
      DetsPlux.put(var!(tx), to_balance_key, {balance2 + amount, lock2})
      DetsPlux.put(var!(tx), validator_balance_key, {balance3 + fees, lock3})

      TokenSupply.subtract(var!(supply), remove)
    end
  end

  defmacro fees(fees, remove) do
    quote bind_quoted: [fees: fees, remove: remove, token: @token], location: :keep do
      balance_key = DetsPlux.tuple(var!(from), token)
      validator_balance_key = DetsPlux.tuple(var!(vOwner), token)

      {balance, lock1} = DetsPlux.get_tx(var!(dets), var!(tx), balance_key, {0, 0})

      {balance3, lock3} =
        DetsPlux.get_tx(var!(dets), var!(tx), validator_balance_key, {0, 0})

      DetsPlux.put(var!(tx), balance_key, {balance - fees - remove, lock1})
      DetsPlux.put(var!(tx), validator_balance_key, {balance3 + fees, lock3})
      TokenSupply.subtract(var!(supply), remove)
    end
  end

  defmacro coinbase(account, token, value) do
    quote bind_quoted: [account: account, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(account, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      DetsPlux.put(var!(tx), key, {balance + value, lock})
    end
  end

  defmacro delete(account, token, amount) do
    quote bind_quoted: [account: account, token: token, amount: amount], location: :keep do
      key = DetsPlux.tuple(account, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      DetsPlux.put(var!(tx), key, {balance - amount, lock})
      TokenSupply.subtract(var!(supply), amount)
    end
  end

  defmacro burn(account, token, amount) do
    quote bind_quoted: [account: account, token: token, amount: amount], location: :keep do
      key = DetsPlux.tuple(account, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      DetsPlux.put(var!(tx), key, {balance - amount, lock})
      TokenSupply.subtract(var!(supply), amount)
    end
  end

  defmacro lock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      if balance >= value do
        DetsPlux.put(var!(tx), key, {balance - value, lock + value})
      else
        :error
      end
    end
  end

  defmacro unlock(to, token, value) do
    quote bind_quoted: [to: to, token: token, value: value], location: :keep do
      key = DetsPlux.tuple(to, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      if lock >= value do
        DetsPlux.put(var!(tx), key, {balance + value, lock - value})
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
        {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

        result = balance - value

        if result >= 0 do
          DetsPlux.put(var!(tx), key, {result, lock})

          to_key = DetsPlux.tuple(to, token)
          {balance2, lock2} = DetsPlux.get_tx(var!(dets), var!(tx), to_key, {0, 0})
          DetsPlux.put(var!(tx), to_key, {balance2 + value, lock2})
        else
          :error
        end
      end
    end
  end

  defmacro pay_burn(from, value) do
    quote bind_quoted: [from: from, token: @token, value: value],
          location: :keep do
      key = DetsPlux.tuple(from, token)
      {balance, lock} = DetsPlux.get_tx(var!(dets), var!(tx), key, {0, 0})

      result = balance - value

      if result >= 0 do
        DetsPlux.put(var!(tx), key, {result, lock})

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
      {balance, lock_amount} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      DetsPlux.put(tx, key, {balance + value, lock_amount})
    end
  end
end
