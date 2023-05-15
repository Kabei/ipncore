defmodule Ippan.Func.Balance do
  alias Ippan.Token

  def lock(%{account: account}, token_id, to_id, amount) do
    account_id = account.id

    token =
      TokenStore.lookup(token_id)
      |> Token.to_map()

    if token.owner == account_id and "lock" in token.props do
      BalanceStore.lock(to_id, token_id, amount)
    else
      false
    end
  end

  def unlock(%{account: account}, token_id, to_id, amount) do
    account_id = account.id

    token =
      TokenStore.lookup(token_id)
      |> Token.to_map()

    if token.owner == account_id and "lock" in token.props do
      BalanceStore.unlock(to_id, token_id, amount)
    else
      false
    end
  end
end
