defmodule Ippan.Func.Domain do
  alias Ippan.Domain
  @fullname_max_size 255
  @years_range 1..2
  @token Default.token()
  def new(
        %{account: account, timestamp: timestamp},
        domain_name,
        owner,
        years,
        opts \\ %{}
      )
      when byte_size(domain_name) <= @fullname_max_size and
             years in @years_range do
    map_filter = Map.take(opts, Domain.optionals())
    account_id = Map.get(account, :id)

    cond do
      not Match.ippan_domain?(domain_name) ->
        raise IppanError, "Invalid domain name"

      map_filter != opts ->
        raise IppanError, "Invalid options parameter"

      not Match.account?(owner) ->
        raise IppanError, "Invalid owner parameter"

      true ->
        amount = Domain.price(domain_name, years)

        domain =
          %Domain{
            name: domain_name,
            owner: owner,
            created_at: timestamp,
            updated_at: timestamp
          }
          |> Map.merge(map_filter)
          |> MapUtil.validate_url(:avatar)
          |> MapUtil.validate_email(:email)
          |> Domain.to_list()

        :ok = BalanceStore.send(account.id, Global.get(:owner), @token, amount, timestamp)

        DomainStore.insert(domain)

        {:continue, {:return_money, [account_id, @token, amount, timestamp]}}
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
          map_filter
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
end
