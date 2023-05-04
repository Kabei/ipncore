defmodule Ippan.Func.Account do
  alias Ippan.{Address, Account}

  def new(%{event: event, hash: hash, timestamp: timestamp} = _source, id, pubkey, pkhash, lhmac)
      when byte_size(pubkey) == 897 and
             byte_size(pkhash) == 16 and
             byte_size(lhmac) == 32 and
             byte_size(id) <= 20 do
    case Match.account?(id) do
      true ->
        auth_hash = hash <> pkhash <> lhmac

        account = %Account{
          id: id,
          validator: Default.validator_id(),
          address: Address.hash(pubkey),
          pubkey: pubkey,
          auth_hash: auth_hash,
          created_at: timestamp
        }

        DetsPlus.insert(event.base, account)

      false ->
        raise IppanError, "Invalid ID format"
    end
  end

  def validator(%{account: account, event: event}, validator_id) do
    case ValidatorStore.exists?(validator_id) do
      true ->
        DetsPlus.insert(event.base, %{account | validator: validator_id})

      false ->
        raise IppanError, "Validator not exists"
    end
  end

  def recovery(%{account: account, event: event}, pkhash, pkmac)
      when byte_size(pkhash) == 32 and byte_size(pkmac) == 16 do
    DetsPlus.insert(event.base, %{account | pkhash: pkhash, pkmac: pkmac})
  end
end
