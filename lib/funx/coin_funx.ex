defmodule Ippan.Funx.Coin do
  alias Ippan.Utils
  require Sqlite
  require BalanceStore

  @app Mix.Project.config()[:app]
  @token Application.compile_env(@app, :token)
  @refund_timeout Application.compile_env(@app, :timeout_refund)

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
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)
    tfees = Utils.calc_fees!(fee_type, vfee, amount, size)
    remove = ceil(tfees * 0.3)

    BalanceStore.send(amount)

    if is_validator do
      BalanceStore.delete(from, @token, remove)
    else
      fees = tfees - remove
      BalanceStore.fees(fees, remove)
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

  def multisend(
        %{
          id: from,
          validator: %{fee: vfee, fee_type: fee_type, owner: vOwner},
          size: size
        },
        token_id,
        outputs
      ) do
    is_validator = vOwner == from
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    total =
      Enum.reduce(outputs, 0, fn [to, amount], acc ->
        BalanceStore.send(amount)
        acc + amount
      end)

    tfees = Utils.calc_fees!(fee_type, vfee, total, size)
    remove = ceil(tfees * 0.3)

    if is_validator do
      BalanceStore.delete(from, @token, remove)
    else
      fees = tfees - remove
      BalanceStore.fees(fees, remove)
    end
  end

  def multisend(source, token_id, outputs, _note) do
    multisend(source, token_id, outputs)
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

    BalanceStore.refund(refund_amount)
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
