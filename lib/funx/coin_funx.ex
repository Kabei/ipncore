defmodule Ippan.Funx.Coin do
  alias Ippan.Utils
  require Sqlite
  require BalanceStore

  @token Application.compile_env(:ipnworker, :token)
  # Three days aprox.
  @refund_timeout 3 * 20_000

  def send(
        %{
          id: account_id,
          validator: %{fee: vfee, fee_type: fee_type, owner: owner},
          size: size
        },
        to,
        token_id,
        amount
      ) do
    is_validator = owner == account_id
    is_native_token = @token == token_id
    balance_key = DetsPlux.tuple(account_id, token_id)
    to_balance_key = DetsPlux.tuple(to, token_id)

    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    if is_validator do
      fees = Utils.calc_fees!(fee_type, vfee, amount, size)
      burn = trunc(fees * 0.3)

      case is_native_token do
        true ->
          BalanceStore.subtract(dets, tx, balance_key, amount + burn)
          BalanceStore.income(dets, tx, to_balance_key, amount)

        _ ->
          native_balance_key = DetsPlux.tuple(account_id, @token)
          BalanceStore.subtract(dets, tx, balance_key, amount)
          BalanceStore.subtract(dets, tx, native_balance_key, burn)
          BalanceStore.income(dets, tx, to_balance_key, amount)
      end

      TokenSupply.subtract(supply, burn)
    else
      fees = Utils.calc_fees!(fee_type, vfee, amount, size)
      burn = trunc(fees * 0.3)
      result_fee = fees - burn

      case is_native_token do
        true ->
          BalanceStore.subtract(dets, tx, balance_key, amount + fees)
          BalanceStore.income(dets, tx, to_balance_key, amount)

        _ ->
          native_balance_key = DetsPlux.tuple(account_id, @token)
          BalanceStore.subtract(dets, tx, balance_key, amount)
          BalanceStore.subtract(dets, tx, native_balance_key, fees)
          BalanceStore.income(dets, tx, to_balance_key, amount)
      end

      validator_balance_key = DetsPlux.tuple(owner, @token)
      BalanceStore.income(dets, tx, validator_balance_key, result_fee)
      TokenSupply.subtract(supply, burn)
    end
  end

  def send(source, to, token_id, amount, _note) do
    send(source, to, token_id, amount)
  end

  def send(
        source = %{hash: hash, id: account_id, round: round_id},
        to,
        token_id,
        amount,
        _note,
        true
      ) do
    send(source, to, token_id, amount)

    db_ref = :persistent_term.get(:main_conn)

    Sqlite.step("insert_refund", [
      hash,
      account_id,
      to,
      token_id,
      amount,
      round_id + @refund_timeout
    ])
  end

  def coinbase(%{balance: {dets, tx}}, token_id, outputs) do
    supply = TokenSupply.new(token_id)

    total =
      for [address, value] <- outputs do
        balance_key = DetsPlux.tuple(address, token_id)
        BalanceStore.income(dets, tx, balance_key, value)
        value
      end
      |> Enum.sum()

    TokenSupply.add(supply, total)
  end

  def burn(%{id: account_id, balance: {dets, tx}}, token_id, amount) do
    supply = TokenSupply.new(token_id)

    balance_key = DetsPlux.tuple(account_id, token_id)
    BalanceStore.subtract(dets, tx, balance_key, amount)
    TokenSupply.subtract(supply, amount)
  end

  def refund(
        %{id: account_id, round: round_id},
        hash16
      ) do
    hash = Base.decode16!(hash16, case: :mixed)
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    {:row, [sender_id, token_id, refund_amount]} =
      Sqlite.step("get_delete_refund", [hash, account_id, round_id])

    balance_key = DetsPlux.tuple(sender_id, token_id)
    BalanceStore.income(dets, tx, balance_key, refund_amount)
  end

  def lock(_, to_id, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    key = DetsPlux.tuple(to_id, token_id)

    BalanceStore.lock(dets, tx, key, amount)
  end

  def unlock(_, to_id, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    key = DetsPlux.tuple(to_id, token_id)

    BalanceStore.unlock(dets, tx, key, amount)
  end
end
