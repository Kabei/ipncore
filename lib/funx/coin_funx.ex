defmodule Ippan.Funx.Coin do
  alias Ippan.{Token, Utils}
  require Sqlite
  require BalanceStore
  require Token

  @app Mix.Project.config()[:app]
  @refund_timeout Application.compile_env(@app, :timeout_refund)

  def send(
        %{
          id: from,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          size: size
        },
        to,
        token_id,
        amount
      ) do
    is_validator = vOwner == from
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    tfees = Utils.calc_fees(fa, fb, size)
    reserve = Utils.calc_reserve(tfees)

    BalanceStore.send(amount)

    if is_validator do
      BalanceStore.reserve(reserve)
    else
      fees = tfees - reserve
      BalanceStore.fees(tfees, fees, reserve)
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

  def coinbase(
        %{
          id: from,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          size: size
        },
        token_id,
        outputs
      ) do
    is_validator = vOwner == from
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)
    tfees = Utils.calc_fees(fa, fb, size)
    reserve = Utils.calc_reserve(tfees)

    total =
      for [account, value] <- outputs do
        BalanceStore.coinbase(account, token_id, value)
        value
      end
      |> Enum.sum()

    TokenSupply.add(supply, total)

    if is_validator do
      BalanceStore.reserve(reserve)
    else
      fees = tfees - reserve
      BalanceStore.fees(tfees, fees, reserve)
    end
  end

  def multisend(
        %{
          id: from,
          validator: %{fa: fa, fb: fb, owner: vOwner},
          size: size
        },
        token_id,
        outputs
      ) do
    is_validator = vOwner == from
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    Enum.each(outputs, fn [to, amount] ->
      BalanceStore.send(amount)
    end)

    tfees = Utils.calc_fees(fa, fb, size)
    reserve = Utils.calc_reserve(tfees)

    if is_validator do
      BalanceStore.reserve(reserve)
    else
      fees = tfees - reserve
      BalanceStore.fees(tfees, fees, reserve)
    end
  end

  def multisend(source, token_id, outputs, _note) do
    multisend(source, token_id, outputs)
  end

  def burn(_source, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    BalanceStore.burn(to, token_id, amount)
  end

  def drop(%{id: account_id}, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    BalanceStore.burn(account_id, token_id, amount)
  end

  def reload(%{id: account_id}, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    %{env: %{"reload.amount" => value}} = Token.get(token_id)

    BalanceStore.reload(account_id, token_id, value)
  end

  def refund(%{id: from}, hash16) do
    hash = Base.decode16!(hash16, case: :mixed)
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(:balance)

    case Sqlite.step("get_refund", [hash, from]) do
      {:row, [to, token_id, refund_amount]} ->
        Sqlite.step("delete_refund", [hash, from])
        BalanceStore.refund(from, to, token_id, refund_amount)

      _ ->
        :error
    end
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
