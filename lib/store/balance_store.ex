defmodule BalanceStore do
  defmacro requires(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value] do
      {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      case balance >= value do
        true ->
          DetsPlux.put(tx, key, {balance - value, lock})

        false ->
          raise IppanError, "Insufficient balance"
      end
    end
  end

  def has?(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value] do
      {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      case balance >= value do
        true -> DetsPlux.put(tx, key, {balance - value, lock})
        false -> false
      end
    end
  end

  defmacro income(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock_amount} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      DetsPlux.put(tx, key, {balance + value, lock_amount})
    end
  end

  defmacro subtract(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock_amount} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      case balance >= value do
        true ->
          DetsPlux.put(tx, key, {balance - value, lock_amount})

        false ->
          false
      end
    end
  end

  defmacro reset(tx, key) do
    quote bind_quoted: [tx: tx, key: key], location: :keep do
      DetsPlux.drop(tx, key)
    end
  end

  defmacro pay(dets, tx, key, to_key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, to_key: to_key, value: value],
          location: :keep do
      {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      result = balance - value

      if result >= 0 do
        DetsPlux.put(tx, key, {result, lock})
        #
        {balance2, lock2} = DetsPlux.get_tx(dets, tx, to_key, {0, 0})
        DetsPlux.put(dets, key, {balance2 + value, lock2})
      else
        :error
      end
    end
  end

  defmacro pay!(dets, tx, key, to_key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, to_key: to_key, value: value],
          location: :keep do
      {balance, lock} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      result = balance - value

      if result >= 0 do
        DetsPlux.put(tx, key, {result, lock})

        {balance2, lock2} = DetsPlux.get_tx(dets, tx, to_key, {0, 0})
        DetsPlux.put(dets, {key, {balance2 + value, lock2}})
      else
        raise IppanError, "Insufficient balance"
      end
    end
  end

  defmacro lock(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock_amount} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      if balance >= value do
        DetsPlux.put(tx, key, {balance - value, lock_amount + value})
      else
        :error
      end
    end
  end

  defmacro unlock(dets, tx, key, value) do
    quote bind_quoted: [dets: dets, tx: tx, key: key, value: value], location: :keep do
      {balance, lock_amount} = DetsPlux.get_tx(dets, tx, key, {0, 0})

      if lock_amount >= value do
        DetsPlux.put(tx, key, {balance + value, lock_amount - value})
      else
        :error
      end
    end
  end
end
