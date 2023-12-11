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
      BalanceStore.fee_reserve(vOwner, reserve)
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
      BalanceStore.fee_reserve(vOwner, reserve)
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
      BalanceStore.fee_reserve(vOwner, reserve)
    else
      fees = tfees - reserve
      BalanceStore.fees(tfees, fees, reserve)
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

  def burn(_source, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    supply = TokenSupply.new(token_id)

    BalanceStore.burn(to, token_id, amount)
  end

  def reload(%{id: account_id, round: round_id}, token_id) do
    db_ref = :persistent_term.get(:main_conn)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)
    %{env: env} = Token.get(token_id)
    %{"reload.amount" => value, "reload.times" => times} = env

    target = DetsPlux.tuple(account_id, token_id)
    {balance, map} = DetsPlux.get_cache(dets, tx, target, {0, %{}})
    init_reload = Map.get(map, "initReload", round_id)
    last_reload = Map.get(map, "lastReload", round_id)
    mult = calc_reload_mult(round_id, init_reload, last_reload, times)

    case env do
      %{"expiry" => expiry} ->
        cond do
          round_id - last_reload > expiry ->
            new_map = Map.delete(map, "lastReload")
            DetsPlux.update_element(tx, target, 3, new_map)
            BalanceStore.expired(target, token_id, balance)

          true ->
            new_map =
              map
              |> Map.put("initReload", init_reload)
              |> Map.put("lastReload", round_id)

            DetsPlux.update_element(tx, target, 3, new_map)
            BalanceStore.reload(target, token_id, value * mult)
        end

      _ ->
        new_map =
          map
          |> Map.put("initReload", init_reload)
          |> Map.put("lastReload", round_id)

        DetsPlux.update_element(tx, target, 3, new_map)
        BalanceStore.reload(target, token_id, value * mult)
    end
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

  defp calc_reload_mult(round_id, init_round, _last_round, _times) when round_id == init_round,
    do: 1

  defp calc_reload_mult(round_id, init_round, last_round, times) do
    div(round_id - init_round, times) - div(last_round - init_round, times)
  end
end
