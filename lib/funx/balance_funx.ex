defmodule Ippan.Funx.Balance do
  require BalanceStore

  def lock(%{balance: {dets, balacne_tx}}, to_id, token_id, amount) do
    balance_key = DetsPlux.tuple(to_id, token_id)

    BalanceStore.lock(dets, balacne_tx, balance_key, amount)
  end

  def unlock(%{balance: {dets, balacne_tx}}, to_id, token_id, amount) do
    balance_key = DetsPlux.tuple(to_id, token_id)
    BalanceStore.unlock(dets, balacne_tx, balance_key, amount)
  end
end
