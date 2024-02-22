defmodule Ippan.Funx.Coin do
  alias Ippan.{Token, Utils}
  require Sqlite
  require BalanceStore
  require Token

  @app Mix.Project.config()[:app]
  @refund_timeout Application.compile_env(@app, :timeout_refund)
  @token Application.compile_env(@app, :token)

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

    BalanceStore.pay from, token_id, amount, tfees do
      balance = BalanceStore.make(from, @token)
      balance_to = BalanceStore.load(to, token_id)
      BalanceStore.send(balance_to, amount)

      reserve = Utils.calc_reserve(tfees)
      fees = tfees - reserve

      if is_validator do
        supply = TokenSupply.new(@token)
        BalanceStore.burn(balance, from, @token, fees)
        BalanceStore.reserve(reserve)
      else
        validator_balance = BalanceStore.load(vOwner, @token)
        BalanceStore.fees(validator_balance, fees)
        BalanceStore.reserve(reserve)
      end
    end
  end

  def send(source, to, token_id, amount, _note) do
    send(source, to, token_id, amount)
  end

  def send(
        source = %{hash: hash, id: from, round: round_id},
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
      from,
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
    tfees = Utils.calc_fees(fa, fb, size)

    case BalanceStore.pay_fee(from, vOwner, tfees) do
      :error ->
        :error

      _ ->
        balance = BalanceStore.make(from, @token)

        total =
          for [account, value] <- outputs do
            BalanceStore.coinbase(account, token_id, value)
            value
          end
          |> Enum.sum()

        supply = TokenSupply.new(token_id)
        TokenSupply.add(supply, total)

        reserve = Utils.calc_reserve(tfees)
        fees = tfees - reserve

        if is_validator do
          supply = TokenSupply.new(@token)
          BalanceStore.burn(balance, from, @token, fees)
          BalanceStore.reserve(reserve)
        else
          validator_balance = BalanceStore.load(vOwner, @token)
          BalanceStore.fees(validator_balance, fees)
          BalanceStore.reserve(reserve)
        end
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
    tfees = Utils.calc_fees(fa, fb, size)
    total = Enum.reduce(outputs, 0, fn [_to, amount], acc -> acc + amount end)

    BalanceStore.pay from, token_id, total, tfees do
      balance = BalanceStore.make(from, token_id)

      Enum.each(outputs, fn [to, amount] ->
        BalanceStore.send(to, token_id, amount)
      end)

      reserve = Utils.calc_reserve(tfees)
      fees = tfees - reserve

      if is_validator do
        supply = TokenSupply.new(@token)
        BalanceStore.burn(balance, from, @token, fees)
        BalanceStore.reserve(reserve)
      else
        validator_balance = BalanceStore.load(vOwner, @token)
        BalanceStore.fees(validator_balance, fees)
        BalanceStore.reserve(reserve)
      end
    end
  end

  def multisend(source, token_id, outputs, _note) do
    multisend(source, token_id, outputs)
  end

  def burn(%{id: account_id}, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    BalanceStore.pay_drop(account_id, token_id, amount)
  end

  def burn(_source, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    BalanceStore.pay_burn(to, token_id, amount)
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
      %{"reload.expiry" => expiry} ->
        cond do
          round_id - last_reload > expiry ->
            dets = DetsPlux.get(:balance)
            tx = DetsPlux.tx(dets, :balance)
            new_map = map |> Map.delete("initReload") |> Map.delete("lastReload")
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

  defp calc_reload_mult(round_id, init_round, _last_round, _times) when round_id == init_round,
    do: 1

  defp calc_reload_mult(round_id, init_round, last_round, times) do
    div(round_id - init_round, times) - div(last_round - init_round, times)
  end

  def stream(%{id: from, round: round_id}, to, token_id, amount) do
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    BalanceStore.pay to, token_id, amount do
      balance = BalanceStore.load(from, token_id)
      BalanceStore.stream(balance, amount)

      db_ref = :persistent_term.get(:main_conn)
      SubPay.update(db_ref, from, to, token_id, round_id)
    end
  end

  def auth(
        %{
          id: account_id,
          size: size,
          validator: %{fa: fa, fb: fb, owner: vOwner}
        },
        token_id,
        to,
        auth
      ) do
    fees = Utils.calc_fees(fa, fb, size)
    dets = DetsPlux.get(:balance)
    tx = DetsPlux.tx(dets, :balance)

    case BalanceStore.pay_fee(account_id, vOwner, fees) do
      :error ->
        :error

      _ ->
        key_to = DetsPlux.tuple(to, token_id)
        {_balance, map} = DetsPlux.get_cache(dets, tx, key_to, {0, %{}})

        new_map =
          if auth do
            Map.put(map, "auth", true)
          else
            Map.delete(map, "auth")
          end

        DetsPlux.update_element(tx, key_to, 3, new_map)
    end
  end
end
