defmodule Ippan.Func.Token do
  alias Ippan.Token

  @type result :: Ippan.Request.result()
  @token Application.compile_env(:ipncore, :token)

  def pre_new(
        %{id: account_id, hash: hash,  round: round, timestamp: timestamp},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        opts \\ %{}
      )
      when byte_size(id) <= 10 and byte_size(name) <= 100 and decimal in 0..18 and
             byte_size(symbol) in 0..5 do
    map_filter =
      opts
      |> Map.take(Token.optionals())

    cond do
      not Match.token?(id) ->
        raise IppanError, "Invalid token ID"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      Platform.has_owner?() and not Platform.owner?(account_id) ->
        raise IppanError, "Invalid operation"

      true ->
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

        MessageStore.approve_df(round, timestamp, hash)
    end
  end

  def new(
        %{timestamp: timestamp},
        id,
        owner_id,
        name,
        decimal,
        symbol,
        opts \\ %{}
      ) do
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
        created_at: timestamp
      }
      |> Map.merge(MapUtil.to_atoms(map_filter))
      |> MapUtil.validate_url(:avatar)
      |> MapUtil.validate_any(:opts, Token.props())

    if id == @token do
      result =
        token
        |> Map.put(:updated_at, timestamp)
        |> Token.to_list()
        |> TokenStore.insert_sync()

      Platform.start()

      result
    else
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
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_length_range(:name, 1..100)
        |> MapUtil.validate_url(:avatar)
        |> MapUtil.validate_account(:owner)
        |> Map.put(:updated_at, timestamp)
        |> TokenStore.update(id: id, owner: account_id)
    end
  end

  def delete(%{id: account_id}, id) when byte_size(id) <= 10 do
    TokenStore.delete([id, account_id])
  end
end