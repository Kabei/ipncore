defmodule Ippan.Func.Account do
  alias Ippan.{Address, Account}

  def new(
        %{account: account_id, timestamp: timestamp},
        validator_id,
        pubkey
      )
      when byte_size(pubkey) == 1794 and is_integer(validator_id) do
    pubkey = Base.decode16!(pubkey)

    cond do
      not Match.account?(account_id) ->
        raise IppanError, "Invalid ID format"

      true ->
        %Account{
          id: account_id,
          validator: validator_id,
          address: Address.hash(pubkey),
          pubkey: pubkey,
          created_at: timestamp
        }
        |> Account.to_list()
        |> AccountStore.insert_sync()
    end
  end

  def subscribe(%{account: account}, validator_id) do
    case ValidatorStore.exists?(validator_id) do
      true ->
        AccountStore.update(%{validator: validator_id}, id: account.id)

      false ->
        raise IppanError, "Validator not exists"
    end
  end
end
