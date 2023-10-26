defmodule Ippan.Funx.Token do
  alias Ippan.Token
  require Token
  require Sqlite
  require BalanceStore

  @app Mix.Project.config()[:app]
  @max_tokens Application.compile_env(@app, :max_tokens)

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
      @max_tokens <= Token.total() ->
        :error

      true ->
        case BalanceStore.pay_burn(account_id, EnvStore.token_price()) do
          :error ->
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
          validator: %{owner: vOwner}
        },
        id,
        opts \\ %{}
      )
      when byte_size(id) <= 10 do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    map_filter = Map.take(opts, Token.editable())
    fees = EnvStore.fees()

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
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
