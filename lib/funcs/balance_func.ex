defmodule Ippan.Func.Balance do
  require BalanceStore

  def lock(%{dets: dets}, to_id, token_id, amount) do
    balance_key = BalanceStore.gen_key(to_id, token_id)
    BalanceStore.lock(dets, balance_key, amount)
  end

  def unlock(%{dets: dets}, to_id, token_id, amount) do
    balance_key = BalanceStore.gen_key(to_id, token_id)
    BalanceStore.unlock(dets, balance_key, amount)
  end
end
