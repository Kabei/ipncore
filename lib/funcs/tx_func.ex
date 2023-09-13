defmodule Ippan.Func.Tx do
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
          dets: dets,
          validator: validator,
          size: size
        },
        to,
        token,
        amount,
        _note \\ <<>>
      ) do
    is_validator = validator.owner != account_id
    balance_key = {account_id, token}

    if is_validator do
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)

      case token do
        @token ->
          BalanceStore.subtract(dets, balance_key, amount + burn)
          BalanceStore.income(dets, {to, token}, amount)

        _ ->
          BalanceStore.subtract(dets, balance_key, amount)
          BalanceStore.subtract(dets, {account_id, @token}, burn)
          BalanceStore.income(dets, {to, token}, amount)
      end
    else
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)
      result_fee = fee - burn

      case token do
        @token ->
          BalanceStore.subtract(dets, balance_key, amount + fee)
          BalanceStore.income(dets, {to, token}, amount)

        _ ->
          BalanceStore.subtract(dets, balance_key, amount)
          BalanceStore.subtract(dets, {account_id, @token}, fee)
          BalanceStore.income(dets, {to, token}, amount)
      end

      BalanceStore.income(dets, {validator.owner, @token}, result_fee)
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
    :ok = send(source, to, token, amount, note)

    SqliteStore.step(conn, stmts, "insert_refund", [
      hash,
      account_id,
      to,
      token,
      amount,
      timestamp + @refund_timeout
    ])
  end

  def coinbase(%{dets: dets}, token_id, outputs) do
    for [address, value] <- outputs do
      BalanceStore.income(dets, {address, token_id}, value)
    end
  end

  def burn(%{id: account_id, dets: dets}, token_id, amount) do
    BalanceStore.subtract(dets, {account_id, token_id}, amount)
  end

  def refund(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts, timestamp: timestamp},
        hash16
      ) do
    hash = Base.decode16!(hash16, case: :mixed)

    {:row, [sender_id, token, refund_amount]} =
      SqliteStore.step(conn, stmts, "get_delete_refund", [hash, account_id, timestamp])

    BalanceStore.income(dets, {sender_id, token}, refund_amount)
  end
end
