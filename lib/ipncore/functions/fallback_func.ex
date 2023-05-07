defmodule Ippan.Func.Fallback do
    def return_money(id, token, amount, timestamp) do
      BalanceStore.income(id, token, amount, timestamp)
    end
  end
