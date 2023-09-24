defmodule Ippan.Func.Token do
  alias Ippan.Token
  require SqliteStore
  require BalanceStore

  @type result :: Ippan.Request.result()
  @token Application.compile_env(:ipncore, :token)
  @max_tokens Application.compile_env(:ipncore, :max_tokens)
  @table_name "assets.token"

  def new(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts, timestamp: timestamp},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        max_supply \\ 0,
        opts \\ %{}
      ) do
    cond do
      SqliteStore.exists?(:token, conn, stmts, "exists_token", id) ->
        :error

      @max_tokens > SqliteStore.one(conn, stmts, "total_tokens") ->
        :error

      true ->
        balance_key = BalanceStore.gen_key(account_id, @token)

        case BalanceStore.subtract(dets, balance_key, EnvStore.token_price()) do
          :error ->
            :error

          _ ->
            map_filter =
              Map.take(opts, Token.optionals())

            token =
              %Token{
                id: id,
                owner: owner_id,
                name: name,
                decimal: decimal,
                symbol: symbol,
                max_supply: max_supply,
                created_at: timestamp,
                updated_at: timestamp
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))
              |> Token.to_list()

            SqliteStore.step(conn, stmts, "insert_token", token)
        end
    end
  end

  def update(
        %{id: account_id, conn: conn, dets: dets, timestamp: timestamp, validator: validator},
        id,
        opts \\ %{}
      )
      when byte_size(id) <= 10 do
    map_filter = Map.take(opts, Token.editable())
    fee = EnvStore.network_fee()
    balance_key = BalanceStore.gen_key(account_id, @token)
    balance_validator_key = BalanceStore.gen_key(validator.owner, @token)

    case BalanceStore.pay(dets, balance_key, balance_validator_key, fee) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, timestamp)

        # if @token == id do
        #   Platform.update()
        # end

        SqliteStore.update(conn, @table_name, map, id: id)
    end
  end

  def delete(%{id: account_id, conn: conn, stmts: stmts}, id) do
    if TokenSupply.get(id) == 0 do
      SqliteStore.step(conn, stmts, "delete_token", [id, account_id])
    end
  end
end
