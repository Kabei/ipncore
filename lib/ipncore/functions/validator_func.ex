defmodule Ippan.Func.Validator do
  import Guards
  alias Phoenix.PubSub
  alias Ippan.Validator
  alias Ippan.Request.Source

  @type result :: Ippan.Request.result()

  def new(
        %{timestamp: timestamp},
        id,
        owner_id,
        hostname,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      ) do
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)
    net_pubkey = Fast64.decode64(net_pubkey)

    validator =
      %Validator{
        id: id,
        hostname: hostname,
        name: name,
        pubkey: pubkey,
        net_pubkey: net_pubkey,
        owner: owner_id,
        fee: fee,
        fee_type: fee_type,
        stake: 0,
        created_at: timestamp,
        updated_at: timestamp
      }
      |> Map.merge(MapUtil.to_atoms(map_filter))
      |> MapUtil.validate_url(:avatar)

    validator
    |> Validator.to_list()
    |> ValidatorStore.insert_sync()

    PubSub.broadcast(:network, "validator", {"new", validator})
  end

  @spec pre_new(
          any(),
          number(),
          String.t(),
          String.t(),
          String.t(),
          binary(),
          binary(),
          non_neg_integer(),
          non_neg_integer(),
          map()
        ) :: result()
  def pre_new(
        %{id: account_id, hash: hash, round: round, timestamp: timestamp},
        id,
        owner_id,
        hostname,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      )
      when is_positive(id) and
             byte_size(name) <= 20 and between_size(hostname, 4, 50) and fee_type in 0..2 and
             fee > 0 and is_float(fee) do
    map_filter = Map.take(opts, Validator.optionals())
    pubkey = Fast64.decode64(pubkey)
    net_pubkey = Fast64.decode64(net_pubkey)

    cond do
      fee_type == 0 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      fee_type == 2 and fee < 1 ->
        raise IppanError, "Invalid fee config"

      byte_size(net_pubkey) > 897 ->
        raise IppanError, "Invalid net_pubkey size #{byte_size(net_pubkey)}"

      byte_size(pubkey) > 897 ->
        raise IppanError, "Invalid pubkey size"

      not Match.account?(owner_id) ->
        raise IppanError, "Invalid owner"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      not Match.hostname?(hostname) ->
        raise IppanError, "Invalid hostname"

      not Platform.owner?(account_id) ->
        raise IppanError, "Invalid operation"

      true ->
        %Validator{
          id: id,
          hostname: hostname,
          name: name,
          pubkey: pubkey,
          owner: owner_id,
          fee: fee,
          fee_type: fee_type,
          stake: 0,
          created_at: timestamp,
          updated_at: timestamp
        }
        |> Map.merge(MapUtil.to_atoms(map_filter))
        |> MapUtil.validate_url(:avatar)

        MessageStore.approve_df(round, timestamp, hash)
    end
  end

  @spec update(Source.t(), number(), map()) :: result()
  def update(%{id: account_id, timestamp: timestamp}, id, opts \\ %{}) do
    map_filter = Map.take(opts, Validator.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not ValidatorStore.owner?(id, account_id) ->
        raise IppanError, "Invalid owner"

      true ->
        MapUtil.to_atoms(map_filter)
        |> MapUtil.validate_hostname(:hostname)
        |> MapUtil.validate_length_range(:name, 1..20)
        |> MapUtil.validate_url(:url)
        |> MapUtil.validate_value(:fee, :gt, 0)
        |> MapUtil.validate_range(:fee_type, 0..2)
        |> MapUtil.transform(
          :pubkey,
          fn x ->
            case Fast64.decode64(x) do
              j when byte_size(j) > 897 ->
                raise IppanError, "Invalid pubkey"

              j ->
                j
            end
          end
        )
        |> MapUtil.transform(
          :net_pubkey,
          fn x ->
            case Fast64.decode64(x) do
              j when byte_size(j) > 897 ->
                raise IppanError, "Invalid net_pubkey"

              j ->
                j
            end
          end
        )
        |> Map.put(:updated_at, timestamp)
        |> ValidatorStore.update(id: id)

        PubSub.broadcast(:network, "validator", {"update", Map.put(opts, :id, id)})
    end
  end

  @spec delete(Source.t(), term) :: result()
  def delete(%{id: account_id}, id) do
    cond do
      not Platform.owner?(account_id) and not ValidatorStore.owner?(id, account_id) ->
        raise IppanError, "Invalid owner"

      true ->
        ValidatorStore.delete(id)

        PubSub.broadcast(:network, "validator", {"delete", id})
    end
  end
end
