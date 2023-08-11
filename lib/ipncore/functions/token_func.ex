defmodule Ippan.Func.Token do
  require Global
  alias Ippan.Token

  @type result :: Ippan.Request.result()
  @max_number 9_223_372_036_854_775_807
  @token Application.compile_env(:ipncore, :token)
  @max_tokens Application.compile_env(:ipncore, :max_tokens)

  def pre_new(
        %{id: account_id, hash: hash, round: round, timestamp: timestamp},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        max_supply,
        opts \\ %{}
      )
      when byte_size(id) <= 10 and byte_size(name) <= 100 and decimal in 0..18 and
             byte_size(symbol) in 0..5 and max_supply >= 0 and max_supply <= @max_number do
    map_filter =
      opts
      |> Map.take(Token.optionals())

    cond do
      not Match.token?(id) ->
        raise IppanHighError, "Invalid token ID"

      not Match.account?(owner_id) ->
        raise IppanHighError, "Invalid owner"

      map_filter != opts ->
        raise IppanHighError, "Invalid options parameter"

      @max_tokens < TokenStore.total() ->
        raise IppanHighError, "Maximum tokens exceeded"

      true ->
        price = EnvStore.token_price()

        %Token{
          id: id,
          owner: owner_id,
          name: name,
          decimal: decimal,
          symbol: symbol,
          created_at: timestamp
        }
        |> Map.merge(MapUtil.to_atoms(map_filter))
        |> MapUtil.validate_url(:avatar)
        |> MapUtil.validate_any(:opts, Token.props())

        case BalanceStore.balance(
               account_id,
               @token,
               price
             ) do
          :ok ->
            MessageStore.approve_df(round, timestamp, hash)

          _ ->
            raise IppanError, "Insufficient balance"
        end
    end
  end

  def new(
        %{id: account_id, timestamp: timestamp},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        max_supply,
        opts \\ %{}
      ) do
    cond do
      @max_tokens < TokenStore.total() ->
        raise IppanError, "Maximum tokens exceeded"

      true ->
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
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_any(:opts, Token.props())

        1 = BalanceStore.burn(account_id, @token, EnvStore.token_price(), timestamp)

        token
        |> Token.to_list()
        |> TokenStore.insert_sync()
    end
  end

  def update(%{id: account_id, timestamp: timestamp}, id, opts \\ %{})
      when byte_size(id) <= 10 do
    map_filter = Map.take(opts, Token.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      true ->
        result =
          MapUtil.to_atoms(map_filter)
          |> MapUtil.validate_length_range(:name, 1..100)
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_account(:owner)
          |> Map.put(:updated_at, timestamp)
          |> TokenStore.update(id: id, owner: account_id)

        case result do
          1 ->
            if @token == id do
              Global.update()
            end

          _ ->
            raise IppanError, "Invalid operation"
        end
    end
  end

  def delete(%{id: account_id}, id) when byte_size(id) <= 10 do
    case TokenStore.step_change(:delete, [id, account_id]) do
      1 ->
        :ok

      _ ->
        raise IppanError, "Invalid operation"
    end
  end
end
