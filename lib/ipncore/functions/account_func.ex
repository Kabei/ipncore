defmodule Ippan.Func.Account do
  alias Ippan.{Address, Account}

  def new(
        %{timestamp: timestamp, sig_type: sig_type},
        validator_id,
        pubkey
      )
      when is_integer(validator_id) do
    pubkey = Fast64.decode64(pubkey)

    cond do
      byte_size(pubkey) != 897 or byte_size(pubkey) != 65 ->
        raise IppanError, "Invalid pubkey size"

      validator_id != Global.get(:validator) ->
        raise IppanError, "Invalid Validator ID"

      true ->
        %Account{
          id: Address.hash(sig_type, pubkey),
          validator: validator_id,
          pubkey: pubkey,
          created_at: timestamp
        }
        |> Account.to_list()
        |> AccountStore.insert_sync()
    end
  end

  def subscribe(%{id: account, validator: sub_validator_id}, validator_id) do
    cond do
      sub_validator_id != validator_id ->
        raise IppanError, "Already subscribe"

      not ValidatorStore.exists?(validator_id) ->
        raise IppanError, "Validator not exists"

      true ->
        AccountStore.update(%{validator: validator_id}, id: account.id)
    end
  end
end
