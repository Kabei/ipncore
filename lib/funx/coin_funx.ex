defmodule Ippan.Funx.Coin do
  alias Ippan.Utils
  require Sqlite
  require BalanceStore

  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)
  # Three days aprox.
  @refund_timeout 3 * 18_000

  def send(
        %{
          id: from,
          validator: %{fee: vfee, fee_type: fee_type, owner: vOwner},
          size: size
        },
        to,
        token_id,
        amount
      ) do
    is_validator = vOwner == from
    is_native_token = @token == token_id
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)
    fees = Utils.calc_fees!(fee_type, vfee, amount, size)
    sacrifice = trunc(fees * 0.3)
    fees = fees - sacrifice

    if is_validator do
      case is_native_token do
        true ->
          BalanceStore.send(amount, fees + sacrifice)

        _ ->
          BalanceStore.send(fees, sacrifice)
          BalanceStore.fees(fees, sacrifice)
      end
    else
      case is_native_token do
        true ->
          BalanceStore.send(amount, fees, sacrifice)

        _ ->
          BalanceStore.send(amount)
          BalanceStore.fees(fees, sacrifice)
      end
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

  def coinbase(_source, token_id, outputs) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    total =
      for [account, value] <- outputs do
        BalanceStore.coinbase(account, token_id, value)
        value
      end
      |> Enum.sum()

    TokenSupply.add(supply, total)
  end

  def burn(%{id: account_id}, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    BalanceStore.burn(account_id, token_id, amount)
  end

  def refund(
        %{id: from, round: round_id},
        hash16
      ) do
    hash = Base.decode16!(hash16, case: :mixed)
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    {:row, [to, token_id, refund_amount]} =
      Sqlite.step("get_delete_refund", [hash, from, round_id])

    BalanceStore.send(refund_amount)
  end

  def lock(_source, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    BalanceStore.lock(to, token_id, amount)
  end

  def unlock(_source, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    BalanceStore.unlock(to, token_id, amount)
  end
end
