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
    balance_key = BalanceStore.gen_key(account_id, token)
    to_balance_key = BalanceStore.gen_key(to, token)

    if is_validator do
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)

      case token do
        @token ->
          BalanceStore.subtract(dets, balance_key, amount + burn)
          BalanceStore.income(dets, to_balance_key, amount)

        _ ->
          native_balance_key = BalanceStore.gen_key(account_id, @token)
          BalanceStore.subtract(dets, balance_key, amount)
          BalanceStore.subtract(dets, native_balance_key, burn)
          BalanceStore.income(dets, to_balance_key, amount)
      end

      TokenSupply.subtract(@token, burn)
    else
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)
      result_fee = fee - burn

      case token do
        @token ->
          BalanceStore.subtract(dets, balance_key, amount + fee)
          BalanceStore.income(dets, to_balance_key, amount)

        _ ->
          native_balance_key = BalanceStore.gen_key(account_id, @token)
          BalanceStore.subtract(dets, balance_key, amount)
          BalanceStore.subtract(dets, native_balance_key, fee)
          BalanceStore.income(dets, to_balance_key, amount)
      end

      validator_balance_key = BalanceStore.gen_key(validator.owner, @token)
      BalanceStore.income(dets, validator_balance_key, result_fee)
      TokenSupply.subtract(@token, burn)
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
    total =
      for [address, value] <- outputs do
        balance_key = BalanceStore.gen_key(address, token_id)
        BalanceStore.income(dets, balance_key, value)
        value
      end
      |> Enum.sum()

    TokenSupply.add(token_id, total)
  end

  def burn(%{id: account_id, dets: dets}, token_id, amount) do
    balance_key = BalanceStore.gen_key(account_id, token_id)
    BalanceStore.subtract(dets, balance_key, amount)
    TokenSupply.subtract(token_id, amount)
  end

  def refund(
        %{id: account_id, conn: conn, dets: dets, stmts: stmts, timestamp: timestamp},
        hash16
      ) do
    hash = Base.decode16!(hash16, case: :mixed)

    {:row, [sender_id, token_id, refund_amount]} =
      SqliteStore.step(conn, stmts, "get_delete_refund", [hash, account_id, timestamp])

    balance_key = BalanceStore.gen_key(sender_id, token_id)
    BalanceStore.income(dets, balance_key, refund_amount)
  end
end
