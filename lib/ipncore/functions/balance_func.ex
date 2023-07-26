defmodule Ippan.Func.Balance do
  alias Ippan.Token

  def lock(%{id: account_id}, token_id, to_id, amount) do
    token =
      TokenStore.lookup_map(token_id)
      |> Token.to_map()

    if token.owner == account_id and "lock" in token.props do
      case BalanceStore.lock(to_id, token_id, amount) do
        false -> raise IppanError, "Invalid operation"
        true -> :ok
      end
    else
      raise IppanError, "Invalid operation"
    end
  end

  def unlock(%{id: account_id}, token_id, to_id, amount) do
    token =
      TokenStore.lookup_map(token_id)
      |> Token.to_map()

    if token.owner == account_id and "lock" in token.props do
      case BalanceStore.unlock(to_id, token_id, amount) do
        false -> raise IppanError, "Invalid operation"
        true -> :ok
      end
    else
      raise IppanError, "Invalid operation"
    end
  end
end
