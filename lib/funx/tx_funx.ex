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
          validator: validator,
          size: size
        },
        to,
        token,
        amount,
        _note \\ <<>>
      ) do
    is_validator = validator.owner != account_id
    balance_key = DetsPlux.tuple(account_id, token)
    to_balance_key = DetsPlux.tuple(to, token)

    supply_tx = DetsPlux.tx(:stats, :supply)
    {supply_key, supply} = TokenSupply.fetch(supply_tx, @token)

    if is_validator do
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)

      case token do
        @token ->
          BalanceStore.subtract(dets, tx, balance_key, amount + burn)
          BalanceStore.income(dets, tx, to_balance_key, amount)

        _ ->
          native_balance_key = DetsPlux.tuple(account_id, @token)
          BalanceStore.subtract(dets, tx, balance_key, amount)
          BalanceStore.subtract(dets, tx, native_balance_key, burn)
          BalanceStore.income(dets, tx, to_balance_key, amount)
      end

      TokenSupply.subtract(supply_tx, supply_key, supply, burn)
    else
      fee = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
      burn = trunc(fee * 0.3)
      result_fee = fee - burn

      case token do
        @token ->
          BalanceStore.subtract(dets, tx, balance_key, amount + fee)
          BalanceStore.income(dets, tx, to_balance_key, amount)

        _ ->
          native_balance_key = DetsPlux.tuple(account_id, @token)
          BalanceStore.subtract(dets, tx, balance_key, amount)
          BalanceStore.subtract(dets, tx, native_balance_key, fee)
          BalanceStore.income(dets, tx, to_balance_key, amount)
      end

      validator_balance_key = DetsPlux.tuple(validator.owner, @token)
      BalanceStore.income(dets, tx, validator_balance_key, result_fee)
      TokenSupply.subtract(supply_tx, supply_key, supply, burn)
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

  def coinbase(%{balance: {dets, tx}}, token_id, outputs) do
    supply_tx = DetsPlux.tx(:stats, :supply)
    {supply_key, supply} = TokenSupply.fetch(supply_tx, @token)

    total =
      for [address, value] <- outputs do
        balance_key = DetsPlux.tuple(address, token_id)
        BalanceStore.income(dets, tx, balance_key, value)
        value
      end
      |> Enum.sum()

    TokenSupply.add(supply_tx, supply_key, supply, total)
  end

  def burn(%{id: account_id, balance: {dets, tx}}, token_id, amount) do
    supply_tx = DetsPlux.tx(:stats, :supply)
    {supply_key, supply} = TokenSupply.fetch(supply_tx, token_id)

    balance_key = DetsPlux.tuple(account_id, token_id)
    BalanceStore.subtract(dets, tx, balance_key, amount)
    TokenSupply.subtract(supply_tx, supply_key, supply, amount)
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
