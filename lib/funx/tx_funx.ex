defmodule Ippan.Funx.Tx do
  alias Ippan.Utils
  require SqliteStore
  require BalanceStore

  @token Application.compile_env(:ipncore, :token)
  @refund_timeout :timer.hours(72)

  def send(_, token, outputs)
      when byte_size(token) <= 10 and is_list(outputs) do
    raise IppanError, "multisend no supported yet"
  end

  def send(
        %{
          id: account_id,
          balance: {dets, tx},
          validator: %{owner: owner, fee_type: fee_type, fee: vfee},
          size: size
        },
        to,
        token_id,
        amount,
        _note \\ <<>>
      ) do
    is_validator = owner == account_id
    is_native_token = @token == token_id
    balance_key = DetsPlux.tuple(account_id, token_id)
    to_balance_key = DetsPlux.tuple(to, token_id)

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

  # with refund enabled
  def send_refundable(
        %{id: account_id, conn: conn, stmts: stmts, hash: hash, timestamp: timestamp} = source,
        to,
        token,
        amount,
        note \\ <<>>
      ) do
    send(source, to, token, amount, note)

    SqliteStore.step(conn, stmts, "insert_refund", [
      hash,
      account_id,
      to,
      token,
      amount,
      timestamp + @refund_timeout
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
        %{id: account_id, conn: conn, balance: {dets, tx}, stmts: stmts, timestamp: timestamp},
        hash16
      ) do
    hash = Base.decode16!(hash16, case: :mixed)

    {:row, [sender_id, token_id, refund_amount]} =
      SqliteStore.step(conn, stmts, "get_delete_refund", [hash, account_id, timestamp])

    balance_key = DetsPlux.tuple(sender_id, token_id)
    BalanceStore.income(dets, tx, balance_key, refund_amount)
  end
end
