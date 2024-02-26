defmodule Ippan.Funx.Token do
  alias Ippan.{Token, Utils}
  require Token
  require Sqlite
  require BalanceStore

  @app Mix.Project.config()[:app]
  @max_tokens Application.compile_env(@app, :max_tokens, 0)
  @json Application.compile_env(@app, :json)

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
      @max_tokens != 0 and @max_tokens <= Token.total() ->
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
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        opts \\ %{}
      ) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    map_filter = Map.take(opts, Token.editable())
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        map =
          MapUtil.to_atoms(map_filter)
          |> Map.put(:updated_at, round_id)

        Token.update(map, id)
    end
  end

  def delete(%{id: account_id}, id) do
    db_ref = :persistent_term.get(:main_conn)
    supply = TokenSupply.new(id)

    if TokenSupply.get(supply) == 0 do
      Token.delete(id, account_id)
      TokenSupply.delete(supply)
    end
  end

  def prop_add(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        prop
      ) do
    db_ref = :persistent_term.get(:main_conn)
    token = Token.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)
    props = if(is_list(prop), do: prop, else: [prop])

    case is_nil(token) do
      true ->
        :error

      _false ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = :lists.append(token.props, props)
            map = %{props: @json.encode!(result), updated_at: round_id}
            Token.update(map, id)
        end
    end
  end

  def prop_drop(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        prop
      ) do
    db_ref = :persistent_term.get(:main_conn)
    token = Token.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)
    props = if(is_list(prop), do: prop, else: [prop])

    case is_nil(token) do
      true ->
        :error

      _false ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = token.props -- props
            map = %{props: @json.encode!(result), updated_at: round_id}
            Token.update(map, id)
        end
    end
  end

  def env_put(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        name,
        value
      ) do
    db_ref = :persistent_term.get(:main_conn)
    token = Token.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case is_nil(token) do
      true ->
        :error

      _false ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = Map.put(token.env, name, value)
            map = %{env: CBOR.encode(result), updated_at: round_id}
            Token.update(map, id)
        end
    end
  end

  def env_delete(
        %{
          id: account_id,
          round: round_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        id,
        name
      ) do
    db_ref = :persistent_term.get(:main_conn)
    token = Token.get(id)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)
    fees = Utils.calc_fees(fa, fb, size)

    case is_nil(token) do
      true ->
        :error

      _ ->
        case BalanceStore.pay_fee(account_id, vOwner, fees) do
          :error ->
            :error

          _ ->
            result = Map.delete(token.env, name)
            map = %{env: CBOR.encode(result), updated_at: round_id}
            Token.update(map, id)
        end
    end
  end
end
