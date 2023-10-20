defmodule Ippan.Funx.Token do
  alias Ippan.Token
  require Token
  require Sqlite
  require BalanceStore

  @token Application.compile_env(:ipncore, :token)
  @max_tokens Application.compile_env(:ipncore, :max_tokens)

  def new(
        %{id: account_id, round: round_id},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        max_supply \\ 0,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    cond do
      @max_tokens > Token.total() ->
        :error

      true ->
        balance_key = DetsPlux.tuple(account_id, @token)

        case BalanceStore.subtract(dets, tx, balance_key, EnvStore.token_price()) do
          false ->
            :error

          _true ->
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
                created_at: round_id,
                updated_at: round_id
              }
              |> Map.merge(MapUtil.to_atoms(map_filter))
              |> Token.to_list()

            Token.insert(token)
        end
    end
  end

  def update(
        %{
          id: account_id,
          round: round_id,
          validator: validator
        },
        id,
        opts \\ %{}
      )
      when byte_size(id) <= 10 do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    map_filter = Map.take(opts, Token.editable())
    fees = EnvStore.network_fee()
    balance_key = DetsPlux.tuple(account_id, @token)
    balance_validator_key = DetsPlux.tuple(validator.owner, @token)

    case BalanceStore.pay(dets, tx, balance_key, balance_validator_key, fees) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, round_id)

        Token.update(map, id: id)
    end
  end

  def delete(%{id: account_id}, id) do
    db_ref = :persistent_term.get(:main_conn)
    supply = TokenSupply.new(id)

    if TokenSupply.get(supply) == 0 do
      Token.delete(id, account_id)
    end
  end
end
