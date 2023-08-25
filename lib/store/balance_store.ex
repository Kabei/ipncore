defmodule BalanceStore do
  defmacro has_balance?(dets, key, value) do
    quote do
      {balance, _lock} = DetsPlus.lookup(unquote(dets), unquote(key), {0, 0})

      balance >= unquote(value)
    end
  end

  defmacro can_be_unlock?(dets, key, value) do
    quote do
      {_balance, lock_amount} = DetsPlus.lookup(unquote(dets), unquote(key), {0, 0})

      lock_amount >= unquote(value)
    end
  end

  defmacro lock(dets, key, value) do
    quote bind_quoted: [dets: dets, key: key, value: value] do
      {balance, lock_amount} = DetsPlus.lookup(dets, key)

      if balance >= value do
        DetsPlus.insert(dets, {key, {balance - value, lock_amount + value}})
      else
        :error
      end
    end
  end

  defmacro unlock(dets, key, value) do
    quote bind_quoted: [dets: dets, key: key, value: value] do
      {balance, lock_amount} = DetsPlus.lookup(dets, key)

      if lock_amount >= value do
        DetsPlus.insert(dets, {key, {balance + value, lock_amount - value}})
      else
        :error
      end
    end
  end

  defmacro income(dets, key, value) do
    quote bind_quoted: [dets: dets, key: key, value: value] do
      {balance, lock_amount} = DetsPlus.lookup(dets, key, {0, 0})

      DetsPlus.insert(dets, {key, {balance + value, lock_amount}})
    end
  end

  defmacro subtract(dets, key, value) do
    quote bind_quoted: [dets: dets, key: key, value: value] do
      {balance, lock_amount} = DetsPlus.lookup(dets, key)

      result = balance - value

      if result >= 0 do
        DetsPlus.insert(dets, {key, {result, lock_amount}})
      else
        :error
      end
    end
  end

  defmacro pay(dets, key, to_key, value) do
    quote bind_quoted: [dets: dets, key: key, to_key: to_key, value: value] do
      {balance, lock} = DetsPlus.lookup(dets, key)

      result = balance - value

      if result >= 0 do
        DetsPlus.insert(dets, {key, {result, lock}})

        {balance2, lock2} = DetsPlus.lookup(dets, to_key, {0, 0})
        DetsPlus.insert(dets, {key, {balance2 + value, lock2}})
      else
        :error
      end
    end
  end
end
