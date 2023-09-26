defmodule Ippan.Func.Balance do
  alias Ippan.Token
  require SqliteStore
  require BalanceStore

  def lock(%{id: account_id, conn: conn, stmts: stmts, dets: dets}, to_id, token_id, amount)
      when is_integer(amount) do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)
    balance_key = BalanceStore.gen_key(to_id, token_id)

    cond do
      is_nil(token) ->
        raise IppanError, "TokenID not exists"

      token.owner != account_id ->
        raise IppanError, "unauthorised"

      "lock" not in token.props ->
        raise IppanError, "Invalid property"

      BalanceStore.has_balance?(dets, balance_key, amount) ->
        :ok

      true ->
        raise IppanError, "Invalid operation"
    end
  end

  def unlock(%{id: account_id, conn: conn, stmts: stmts, dets: dets}, to_id, token_id, amount)
      when is_integer(amount) do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)
    balance_key = BalanceStore.gen_key(to_id, token_id)

    cond do
      is_nil(token) ->
        raise IppanError, "TokenID not exists"

      token.owner != account_id ->
        raise IppanError, "unauthorised"

      "lock" not in token.props ->
        raise IppanError, "Invalid property"

      BalanceStore.can_be_unlock?(dets, balance_key, amount) ->
        :ok

      true ->
        raise IppanError, "Invalid operation"
    end
  end
end
