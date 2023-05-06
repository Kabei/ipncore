defmodule Ippan.Func.Token do
  alias Ippan.Token

  @type result :: Ippan.Request.result()

  @token Default.token()

  def new(%{account: account}, id, owner_id, name, decimal, symbol, opts \\ %{})
      when byte_size(id) <= 10 and byte_size(name) <= 100 and decimal in 0..18 and
             byte_size(symbol) <= 5 do
    map_filter = Map.take(opts, Token.optionals())

    cond do
      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      Platform.owner?(account.id) ->
        raise IppanError, "Invalid operation"

      true ->
        :done =
          %Token{
            id: id,
            owner: owner_id,
            name: name,
            symbol: symbol
          }
          |> Map.merge(map_filter)
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_any(:opts, Token.props())
          |> Token.to_list()
          |> TokenStore.insert()

        if id == @token do
          Platform.start()
        end

        :ok
    end
  end

  def update(%{account: account, timestamp: timestamp}, id, opts \\ %{})
      when byte_size(id) <= 10 do
    map_filter = Map.take(opts, Token.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      true ->
        map_filter
        |> MapUtil.validate_length_range(:name, 1..100)
        |> MapUtil.validate_url(:avatar)
        |> MapUtil.validate_account(:owner)
        |> Map.put(:updated_at, timestamp)
        |> TokenStore.update(id: id, owner: account.id)
    end
  end

  def delete(%{account: account}, id) when byte_size(id) <= 10 do
    TokenStore.delete([id, account.id])
  end
end
