defmodule Ippan.Func.Balance do
  require BalanceStore

  def lock(%{dets: dets}, to_id, token_id, amount) do
    BalanceStore.lock(dets, {to_id, token_id}, amount)
  end

  def unlock(%{dets: dets}, to_id, token_id, amount) do
    BalanceStore.unlock(dets, {to_id, token_id}, amount)
  end
end
