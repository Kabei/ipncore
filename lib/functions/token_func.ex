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
        max_supply,
        opts \\ %{}
      ) do
    cond do
      SqliteStore.exists?(:token, conn, stmts, "exists_token", id) ->
        :error

      @max_tokens > SqliteStore.total(conn, stmts, "total_token") ->
        :error

      true ->
        case BalanceStore.subtract(dets, {account_id, @token}, EnvStore.token_price()) do
          :error ->
            :error

          _ ->
            map_filter =
              opts
              |> Map.take(Token.optionals())

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
    balance = {account_id, @token}
    validator_balance = {validator.owner, @token}

    case BalanceStore.pay(dets, balance, validator_balance, fee) do
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
    SqliteStore.step(conn, stmts, "delete_token", [id, account_id])
  end
end
