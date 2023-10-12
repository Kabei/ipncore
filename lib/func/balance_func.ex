defmodule Ippan.Func.Balance do
  alias Ippan.Token
  require SqliteStore
  require BalanceStore

  def lock(%{id: account_id, conn: conn, stmts: stmts, balance: {dets, tx}}, to_id, token_id, amount)
      when is_integer(amount) do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)
    balance_key = DetsPlux.tuple(to_id, token_id)

    cond do
      is_nil(token) ->
        raise IppanError, "TokenID not exists"

      token.owner != account_id ->
        raise IppanError, "unauthorised"

      "lock" not in token.props ->
        raise IppanError, "Invalid property"

        BalanceStore.requires!(dets, tx, balance_key, amount)
    end
  end

  def unlock(
        %{id: account_id, conn: conn, stmts: stmts, balance: {dets, tx}},
        to_id,
        token_id,
        amount
      )
      when is_integer(amount) do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)
    balance_key = DetsPlux.tuple(to_id, token_id)

    cond do
      is_nil(token) ->
        raise IppanError, "TokenID not exists"

      token.owner != account_id ->
        raise IppanError, "unauthorised"

      "lock" not in token.props ->
        raise IppanError, "Invalid property"

      true ->
        BalanceStore.requires!(dets, tx, balance_key, amount)
    end
  end
end
