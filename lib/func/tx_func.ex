defmodule Ippan.Func.Tx do
  alias Ippan.Utils
  alias Ippan.Token
  require SqliteStore
  require BalanceStore
  require Logger

  @max_tx_amount Application.compile_env(:ipnworker, :max_tx_amount)
  @note_max_size Application.compile_env(:ipnworker, :note_max_size)
  @token Application.compile_env(:ipnworker, :token)

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
        token_id,
        amount,
        note \\ <<>>
      )
      when is_integer(amount) and amount <= @max_tx_amount and
             account_id != to and
             byte_size(note) <= @note_max_size do
    fee_amount = Utils.calc_fees!(validator.fee_type, validator.fee, amount, size)
    balance_key = DetsPlux.tuple(account_id, token_id)

    if @token == token_id do
      BalanceStore.has?(dets, tx, balance_key, amount + fee_amount)
    else
      native_balance_key = DetsPlux.tuple(account_id, @token)

      BalanceStore.has?(dets, tx, balance_key, amount) and
        BalanceStore.has?(dets, tx, native_balance_key, fee_amount)
    end
    |> case do
      true ->
        :ok

      false ->
        raise IppanError, "Insufficient balance"
    end
  end

  # with refund enabled
  def send_refundable(source, to, token, amount) do
    send(source, to, token, amount)
  end

  def coinbase(%{id: account_id, conn: conn, stmts: stmts}, token_id, outputs)
      when length(outputs) > 0 do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)

    cond do
      token.owner != account_id ->
        raise IppanError, "Invalid owner"

      "coinbase" not in token.props ->
        raise IppanError, "Token property invalid"

      true ->
        total =
          Enum.reduce(outputs, 0, fn [_account_id, amount], acc ->
            cond do
              amount <= 0 ->
                raise ArgumentError, "Amount must be positive number"

              amount > @max_tx_amount ->
                raise ArgumentError, "Amount exceeded max value"

              not Match.account?(account_id) ->
                raise ArgumentError, "Account ID invalid"

              true ->
                amount + acc
            end
          end)

        tx = DetsPlux.tx(:suplly)
        supply = TokenSupply.get(tx, token_id)

        if token.max_supply < total + supply do
          raise ArgumentError, "Max supply exceeded"
        end

        :ok
    end
  end

  def burn(%{id: account_id, conn: conn, balance: {dets, tx}, stmts: stmts}, token_id, amount)
      when is_integer(amount) and amount > 0 do
    token = SqliteStore.lookup_map(:token, conn, stmts, "get_token", [token_id], Token)
    balance_key = DetsPlux.tuple(account_id, token_id)

    cond do
      "burn" not in token.props ->
        raise IppanError, "Token property invalid"

      not BalanceStore.has?(dets, tx, balance_key, amount) ->
        raise IppanError, "Insufficient balance"

      true ->
        :ok
    end
  end

  def refund(%{id: account_id, conn: conn, stmts: stmts, timestamp: timestamp}, hash16)
      when byte_size(hash16) == 64 do
    hash = Base.decode16!(hash16, case: :mixed)

    case SqliteStore.exists?(conn, stmts, "exists_refund", [hash, account_id, timestamp]) do
      false ->
        raise IppanError, "Hash refund not exists"

      true ->
        :ok
    end
  end
end
