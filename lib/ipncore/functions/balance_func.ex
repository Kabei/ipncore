defmodule Ippan.Func.Balance do
  def lock(%{id: account_id}, token_id, to_id, amount) do
    token =
      TokenStore.lookup_map(token_id)

    if token.owner == account_id and "lock" in token.props do
      case BalanceStore.lock(to_id, token_id, amount) do
        1 -> :ok
        _ -> raise IppanError, "Invalid operation"
      end
    else
      raise IppanError, "Invalid operation"
    end
  end

  def unlock(%{id: account_id}, token_id, to_id, amount) do
    token =
      TokenStore.lookup_map(token_id)

    if token.owner == account_id and "lock" in token.props do
      case BalanceStore.unlock(to_id, token_id, amount) do
        1 -> :ok
        _ -> raise IppanError, "Invalid operation"
      end
    else
      raise IppanError, "Invalid operation"
    end
  end
end
