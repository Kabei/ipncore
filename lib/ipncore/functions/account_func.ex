defmodule Ippan.Func.Account do
  alias Ippan.{Address, Account}

  def new(
        %{account: account_id, timestamp: timestamp},
        validator_id,
        pubkey,
        pkhash,
        pkhash2,
        lhmac
      )
      when byte_size(pubkey) == 1794 and is_integer(validator_id) do
    pubkey = Base.decode16!(pubkey)
    pkhash = Base.decode16!(pkhash)
    pkhash2 = Base.decode16!(pkhash2)
    lhmac = Base.decode16!(lhmac)
    auth_hash = :binary.list_to_bin([pkhash, pkhash2, lhmac])

    cond do
      byte_size(auth_hash) != 80 ->
        raise IppanError, "Invalid arguments"

      not Match.account?(account_id) ->
        raise IppanError, "Invalid ID format"

      true ->
        %Account{
          id: account_id,
          validator: validator_id,
          address: Address.hash(pubkey),
          pubkey: pubkey,
          auth_hash: auth_hash,
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

  def recovery(%{account: account, hash: hash}, pkhash, pkhash2, pkmac)
      when byte_size(pkhash) == 64 and byte_size(pkhash2) == 64 and byte_size(pkmac) == 32 do
    pkhash = Base.decode16!(pkhash)
    pkhash2 = Base.decode16!(pkhash2)
    pkmac = Base.decode16!(pkmac)
    auth_hash = :binary.list_to_bin([pkhash, pkhash2, hash, pkmac])

    AccountStore.update(%{auth_hash: auth_hash}, id: account.id)
  end
end
