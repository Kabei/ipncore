defmodule Ippan.Funx.Account do
  alias Ippan.{Utils, Validator}
  require BalanceStore
  require Sqlite
  require Validator

  def new(%{id: account_id}, pubkey, sig_type, validator_id, fa, fb) do
    pubkey = Fast64.decode64(pubkey)
    tx = DetsPlux.tx(:wallet)
    db_ref = :persistent_term.get(:main_conn)

    DetsPlux.put(
      tx,
      {account_id, pubkey, sig_type, %{"fa" => fa, "fb" => fb, "vid" => validator_id}}
    )

    Validator.count_sub(validator_id, 1)
  end

  def subscribe(
        %{
          id: from,
          validator: %{owner: vOwner},
          size: size
        },
        validator_id,
        fa,
        fb
      ) do
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    fees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(from, vOwner, fees) do
      :error ->
        :error

      _ ->
        db_ref = :persistent_term.get(:main_conn)
        wallet = DetsPlux.get(:wallet)
        wtx = DetsPlux.tx(wallet, :wallet)
        {_pk, _, %{"vid" => old_vid}} = DetsPlux.get_cache(wallet, wtx, "@ippan")
        DetsPlux.update_element(wtx, from, 4, %{"fa" => fa, "fb" => fb, "vid" => validator_id})
        Validator.count_sub(validator_id, 1)
        Validator.count_sub(old_vid, -1)
    end
  end

  def edit_key(
        %{
          id: from,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          size: size
        },
        pubkey,
        sig_type
      ) do
    db = DetsPlux.get(:balance)
    tx = DetsPlux.tx(db, :balance)
    fees = Utils.calc_fees(fa, fb, size)
    pubkey = Fast64.decode64(pubkey)

    case BalanceStore.pay_fee(from, vOwner, fees) do
      :error ->
        :error

      _ ->
        wallet = DetsPlux.get(:wallet)
        wtx = DetsPlux.tx(wallet, :wallet)
        DetsPlux.get_cache(wallet, wtx, from)
        DetsPlux.update_element(wtx, from, 2, pubkey)
        DetsPlux.update_element(wtx, from, 3, sig_type)
    end
  end
end
