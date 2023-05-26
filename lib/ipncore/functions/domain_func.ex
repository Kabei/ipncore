defmodule Ippan.Func.Domain do
  alias Ippan.Domain
  @fullname_max_size 255
  @years_range 1..2
  @token Default.token()

  def new(
        %{account: account, hash: hash, timestamp: timestamp},
        domain_name,
        owner,
        years,
        opts \\ %{}
      )
      when byte_size(domain_name) <= @fullname_max_size and
             years in @years_range do
    account_id = account.id
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
        amount = Domain.price(domain_name, years)

        current_round = 0

        domain =
          %Domain{
            name: domain_name,
            owner: owner,
            created_at: timestamp,
            renewed_at: timestamp + years * 31_536_000_000,
            updated_at: timestamp
          }
          |> Map.merge(MapUtil.to_atoms(map_filter))
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_email(:email)
          |> Domain.to_list_def(hash, current_round)

        chain_owner = Global.get(:owner)

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

  def update(%{account: account, timestamp: timestamp}, domain_name, opts \\ %{}) do
    map_filter = Map.take(opts, Domain.editable())

    cond do
      opts == %{} ->
        raise IppanError, "options is empty"

      map_filter != opts ->
        raise IppanError, "Invalid option field"

      not DomainStore.owner?(domain_name, account.id) ->
        raise IppanError, "Invalid owner"

      true ->
        validator = ValidatorStore.lookup(account.validator)
        fees = EnvStore.get("fee_update", 500)
        :ok = BalanceStore.send_fees(account.id, validator.owner, fees, timestamp)

        1 =
          MapUtil.to_atoms(map_filter)
          |> MapUtil.validate_account(:owner)
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_email(:email)
          |> Map.put(:updated_at, timestamp)
          |> DomainStore.update(name: domain_name)
    end
  end

  def delete(%{account: account}, domain_name) do
    DomainStore.delete([domain_name, account.id])
  end

  # def renew(%{account: account, timestamp: timestamp}, years) do
  #   cond do
  #     not DomainStore.owner?(domain_name, account.id) ->
  #       raise IppanError, "Invalid owner"

  #   end
  # end
end
