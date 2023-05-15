defmodule Ippan.Func.Validator do
  import Guards
  alias Ippan.Validator
  alias Ippan.Request.Source

  @type result :: Ippan.Request.result()

  @spec new(
          Source.t(),
          number(),
          String.t(),
          String.t(),
          String.t(),
          binary(),
          non_neg_integer(),
          non_neg_integer(),
          map()
        ) :: result()
  def new(
        %{account: account},
        id,
        owner_id,
        hostname,
        name,
        pubkey,
        fee_type,
        fee,
        opts \\ %{}
      )
      when is_positive(id) and
             byte_size(name) <= 20 and between_size(hostname, 4, 50) and fee_type in 0..2 and fee > 0 do
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)

    cond do
      byte_size(pubkey) != 897 ->
        raise IppanError, "Invalid pubkey size"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      Match.domain?(hostname) ->
        raise IppanError, "Invalid hostname"

      Platform.owner?(account.id) ->
        raise IppanError, "Invalid operation"

      true ->
        object =
          %Validator{
            id: id,
            hostname: hostname,
            name: name,
            pubkey: pubkey,
            owner: owner_id,
            fee: fee,
            fee_type: fee_type
          }
          |> Map.merge(map_filter)
          |> MapUtil.validate_url(:avatar)

        ValidatorStore.insert(Validator.to_list(object))

        {:notify, object}
    end
  end

  @spec update(Source.t(), number(), map()) :: result()
  def update(%{account: account, timestamp: timestamp}, id, opts \\ %{}) do
    map_filter = Map.take(opts, Validator.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not ValidatorStore.owner?(id, account.id) ->
        raise IppanError, "Invalid owner"

      true ->
        map_filter
        |> MapUtil.validate_hostname(:hostname)
        |> MapUtil.validate_length_range(:name, 1..20)
        |> MapUtil.validate_url(:url)
        |> MapUtil.validate_value(:fee, :gt, 0)
        |> MapUtil.validate_range(:fee_type, 0..2)
        |> MapUtil.transform(
          :pubkey,
          fn x ->
            case Fast64.decode64(x) do
              j when byte_size(j) != 897 ->
                raise IppanError, "Invalid pubkey"

              j ->
                j
            end
          end
        )
        |> Map.put(:updated_at, timestamp)
        |> ValidatorStore.update(id: id)

        {:notify, {id, map_filter}}
    end
  end

  @spec delete(Source.t(), term) :: result()
  def delete(%{account: account}, id) do
    cond do
      not ValidatorStore.owner?(id, account.id) ->
        raise IppanError, "Invalid owner"

      true ->
        ValidatorStore.delete([id, account.id])

        {:notify, id}
    end
  end
end
