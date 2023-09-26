defmodule Ippan.Func.Token do
  alias Ippan.Token
  require SqliteStore
  require BalanceStore

  @type result :: Ippan.Request.result()
  @max_number 1_000_000_000_000_000_000_000_000_000
  @token Application.compile_env(:ipnworker, :token)
  @max_tokens Application.compile_env(:ipnworker, :max_tokens)

  def new(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        max_supply \\ 0,
        opts \\ %{}
      )
      when byte_size(id) <= 10 and byte_size(name) <= 100 and decimal in 0..18 and
             byte_size(symbol) in 0..5 and max_supply >= 0 and max_supply <= @max_number do
    map_filter =
      opts
      |> Map.take(Token.optionals())

    cond do
      not Match.token?(id) ->
        raise IppanError, "Invalid token ID"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner argument"

      map_filter != opts ->
        raise IppanError, "Invalid option arguments"

      SqliteStore.exists?(conn, stmts, "exists_token", id) ->
        raise IppanError, "Token already exists"

      @max_tokens < SqliteStore.one(conn, stmts, "total_tokens") ->
        raise IppanError, "Maximum tokens exceeded"

      true ->
        price = EnvStore.token_price()

        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_url(:avatar)
        |> MapUtil.validate_any(:opts, Token.props())

        balance_key = BalanceStore.gen_key(account_id, @token)

        case BalanceStore.has_balance?(dets, balance_key, price) do
          false ->
            raise IppanError, "Insufficient balance"

          true ->
            :ok
        end
    end
  end

  def update(%{id: account_id, conn: conn, stmts: stmts}, id, opts \\ %{})
      when byte_size(id) <= 10 do
    map_filter = Map.take(opts, Token.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not SqliteStore.exists?(conn, stmts, "owner_token", [id, account_id]) ->
        raise IppanError, "Invalid owner"

      true ->
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_length_range(:name, 1..100)
        |> MapUtil.validate_url(:avatar)
        |> MapUtil.validate_account(:owner)
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, id) when byte_size(id) <= 10 do
    tx = DetsPlux.tx(:supply)

    cond do
      TokenSupply.get(tx, id) != 0 ->
        raise IppanError, "Invalid operation"

      not SqliteStore.exists?(conn, stmts, "owner_token", [id, account_id]) ->
        raise IppanError, "Invalid owner"

      true ->
        :ok
    end
  end
end
