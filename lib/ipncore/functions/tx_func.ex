defmodule Ippan.Func.Tx do
  alias Exqlite.Sqlite3NIF
  alias Ippan.Utils
  require Logger

  @max_tx_amount Application.compile_env(:ipncore, :max_tx_amount)
  @refund_timeout :timer.hours(72)
  @note_max_size Application.compile_env(:ipncore, :note_max_size)

  def send(_, token, outputs)
      when byte_size(token) <= 10 and is_list(outputs) do
    raise IppanError, "multisend no supported yet"
  end

  def send(
        %{
          id: account_id,
          validator: validator_id,
          timestamp: timestamp,
          size: size
        },
        to,
        token,
        amount,
        note \\ <<>>
      )
      when amount <= @max_tx_amount and
             account_id != to and
             byte_size(note) <= @note_max_size do
    %{fee: fee, fee_type: fee_type, owner: validator_owner} =
      ValidatorStore.lookup_map(validator_id)

    fee_amount = Utils.calc_fees!(fee_type, fee, amount, size)

    :ok =
      BalanceStore.transaction(
        account_id,
        to,
        token,
        amount,
        validator_owner,
        fee_amount,
        timestamp
      )
  end

  # with refund enabled
  def send_refundable(source, to, token, amount) do
    :ok = send(source, to, token, amount)

    %{id: account_id, hash: hash, timestamp: timestamp} = source

    RefundStore.insert([
      hash,
      account_id,
      to,
      token,
      amount,
      timestamp + @refund_timeout
    ])
  end

  def coinbase(%{id: account_id, hash: hash, timestamp: timestamp}, token, outputs)
      when length(outputs) > 0 do
    hash16 = Base.encode16(hash, case: :lower)

    case TokenStore.step("owner_props", [token, account_id, "%coinbase%"]) do
      {:row, [1]} ->
        BalanceStore.run(fn %{conn: conn, stmt: stmt} = state ->
          try do
            Sqlite3NIF.execute(conn, ~c"SAVEPOINT #{hash16}")
            statment = Map.get(stmt, :income)

            total =
              Enum.reduce(outputs, 0, fn [account_id, amount], acc ->
                cond do
                  amount <= 0 ->
                    raise ArgumentError, "Amount must be positive number"

                  amount > @max_tx_amount ->
                    raise ArgumentError, "Amount exceeded max value"

                  not Match.account?(account_id) ->
                    raise ArgumentError, "Account ID invalid"

                  true ->
                    BalanceStore.receive(
                      conn,
                      statment,
                      account_id,
                      token,
                      amount,
                      timestamp
                    )
                end

                acc + amount
              end)

            # Logger.info(inspect(total))

            # sum supply
            case TokenStore.step_change("sum_supply", [token, total]) do
              1 ->
                Sqlite3NIF.execute(conn, ~c"RELEASE #{hash16}")

              0 ->
                Sqlite3NIF.execute(conn, ~c"ROLLBACK TO #{hash16}")
                raise ArgumentError, "Max supply exceeded"
            end

            {:reply, :ok, state}
          rescue
            ex ->
              Logger.error(Exception.format(:error, ex, __STACKTRACE__))
              Sqlite3NIF.execute(conn, ~c"ROLLBACK TO #{hash16}")
              {:reply, {:error, ex.message}, state}
          end
        end)

      _ ->
        raise IppanError, "Invalid owner"
    end
  end

  def burn(%{id: account_id, timestamp: timestamp}, token, amount) do
    case TokenStore.step("props", [token, "%burn%"]) do
      {:row, [1]} ->
        case BalanceStore.burn(account_id, token, amount, timestamp) do
          1 ->
            TokenStore.step("sum_burned", [token, amount])

          _ ->
            raise IppanError, "Invalid operation"
        end

      _ ->
        raise IppanError, "Invalid operation"
    end
  end

  def refund(%{id: account_id, timestamp: timestamp}, hash16)
      when byte_size(hash16) == 64 do
    hash = Base.decode16!(hash16, case: :mixed)

    [sender_id, token, refund_amount] = RefundStore.one([hash, account_id, timestamp])

    case BalanceStore.send(account_id, sender_id, token, refund_amount, timestamp) do
      :ok ->
        RefundStore.delete([hash])

      _ ->
        raise IppanError, "Invalid operation"
    end
  end
end
