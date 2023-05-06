defmodule Ippan.Func.Account do
  alias Ippan.{Address, Account}

  def new(
        %{account: account_id, hash: hash, timestamp: timestamp},
        pubkey,
        pkhash,
        pkhash2,
        lhmac
      )
      when byte_size(account_id) <= 20 and
             byte_size(pubkey) == 897 and
             byte_size(pkhash) == 32 and
             byte_size(pkhash2) == 32 and
             byte_size(lhmac) == 16 do
    case Match.account?(account_id) do
      true ->
        %Account{
          id: account_id,
          validator: Default.validator_id(),
          address: Address.hash(pubkey),
          pubkey: pubkey,
          auth_hash: :binary.list_to_bin([pkhash, pkhash2, hash, lhmac]),
          created_at: timestamp
        }
        |> Account.to_list()
        |> AccountStore.insert()

      # |> Map.from_struct()

      # DetsPlus.insert(event.base, account)

      false ->
        raise ArgumentError, "Invalid ID format"
    end
  end

  def validator(%{account: account}, validator_id) do
    case ValidatorStore.exists?(validator_id) do
      true ->
        AccountStore.update(%{validator: validator_id}, id: account.id)

      false ->
        raise IppanError, "Validator not exists"
    end
  end

  def recovery(%{account: account, hash: hash}, pkhash, pkhash2, pkmac)
      when byte_size(pkhash) == 32 and byte_size(pkhash2) == 32 and byte_size(pkmac) == 16 do
    auth_hash = :binary.list_to_bin([pkhash, pkhash2, hash, pkmac])

    AccountStore.update(%{auth_hash: auth_hash}, id: account.id)
  end
end
