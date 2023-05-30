defmodule Ippan.Func.Domain do
  alias Ippan.Domain
  @fullname_max_size 255
  @token Default.token()

  def new(
        %{id: account_id, hash: hash, timestamp: timestamp},
        domain_name,
        owner,
        days,
        opts \\ %{}
      )
      when byte_size(domain_name) <= @fullname_max_size and
             days > 0 do
    map_filter = Map.take(opts, Domain.optionals())

    cond do
      not Match.ippan_domain?(domain_name) ->
        raise IppanError, "Invalid domain name"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      not Match.account?(owner) ->
        raise IppanError, "Invalid owner parameter"

      DomainStore.exists?(domain_name) ->
        raise IppanError, "domain already has a owner"

      true ->
        amount = Domain.price(domain_name, days)
        current_round = 0
        chain_owner = Global.get(:owner)

        domain =
          %Domain{
            name: domain_name,
            owner: owner,
            created_at: timestamp,
            renewed_at: timestamp + days * 86_400_000,
            updated_at: timestamp
          }
          |> Map.merge(MapUtil.to_atoms(map_filter))
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_email(:email)
          |> Domain.to_list_def(hash, current_round)

        case BalanceStore.deferred(
               domain_name,
               400,
               account_id,
               chain_owner,
               @token,
               amount,
               timestamp,
               hash,
               current_round
             ) do
          :ok ->
            DomainStore.insert_deferred(domain)

          0 ->
            raise IppanError, "Resource already taken"

          :error ->
            raise IppanError, "Insufficient balance"
        end
    end
  end

  def update(
        %{id: account_id, validator: validator_id, timestamp: timestamp},
        domain_name,
        opts \\ %{}
      ) do
    map_filter = Map.take(opts, Domain.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not DomainStore.owner?(domain_name, account_id) ->
        raise IppanError, "Invalid owner"

      true ->
        validator = ValidatorStore.lookup([validator_id])
        fees = EnvStore.get("fee_update", 500)
        :ok = BalanceStore.send_fees(account_id, validator.owner, fees, timestamp)

        1 =
          MapUtil.to_atoms(map_filter)
          |> MapUtil.validate_account(:owner)
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_email(:email)
          |> Map.put(:updated_at, timestamp)
          |> DomainStore.update(name: domain_name)
    end
  end

  def delete(%{id: account_id}, domain_name) do
    DomainStore.delete([domain_name, account_id])
  end

  def renew(%{id: account_id, timestamp: timestamp}, name, days)
      when is_integer(days) and days > 0 do
    amount = Domain.price(name, days)
    chain_owner = Global.get(:owner)

    cond do
      not DomainStore.owner?(name, account_id) ->
        raise IppanError, "Invalid owner"

      BalanceStore.send(account_id, chain_owner, @token, amount, timestamp) != :ok ->
        raise IppanError, "Insufficient balance"

      DomainStore.renew(name, account_id, days * 86_400_000, timestamp) != 1 ->
        raise IppanError, "Invalid operation"

      true ->
        :ok
    end
  end
end
