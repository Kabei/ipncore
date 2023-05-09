defmodule Ippan.Func.Tx do
  @max_tx_amount 1_000_000_000_000_000
  alias Exqlite.Sqlite3
  alias Ippan.Utils

  # @token Default.token()

  def send(_, token, outputs)
      when byte_size(token) <= 10 and is_list(outputs) do
    raise IppanError, "multisend no supported yet"
  end

  def send(
        %{
          account: account,
          hash: hash,
          timestamp: timestamp,
          size: size
        },
        to,
        token,
        amount
      )
      when byte_size(token) <= 10 and amount <= @max_tx_amount and account.id != to do
    # hash16 = Base.encode16(hash)
    account_id = Map.get(account, :id)
    validator_id = Map.get(account, :id)

    %{fee: fee, fee_type: fee_type, owner: validator_owner} = ValidatorStore.lookup(validator_id)

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

    RefundStore.replace([hash, account_id, to, token, amount, timestamp + :timer.hours(72)])
  end

  def coinbase(%{account: account, hash: hash, timestamp: timestamp}, token, outputs) do
    hash16 = Base.encode16(hash)

    case TokenStore.execute_prepare(:owner_props, [token, account.id, "%coinbase%"]) do
      true ->
        BalanceStore.launch(fn %{conn: conn, stmt: stmt} = state ->
          try do
            Sqlite3.execute(conn, "SAVEPOINT #{hash16}")
            statment = Map.get(stmt, :income)

            total =
              Stream.transform(outputs, 0, fn [account_id, amount], acc ->
                if Match.account?(account_id) do
                  BalanceStore.receive(conn, statment, account_id, token, amount, timestamp)
                else
                  raise ArgumentError, "Account ID invalid"
                end

                acc + amount
              end)
              |> Enum.to_list()
              |> List.first()

            # sum supply
            TokenStore.execute_prepare(:sum_supply, [token, total])
            Sqlite3.execute(conn, "RELEASE #{hash16}")
            {:reply, :ok, state}
          rescue
            ex ->
              Sqlite3.execute(conn, "ROLLBACK TO #{hash16}")
              {:reply, {:error, ex.message}, state}
          end
        end)

      false ->
        raise IppanError, "Invalid owner"
    end
  end

  def burn(%{account: account, timestamp: timestamp}, token, amount) do
    account_id = account.id

    case TokenStore.execute_prepare(:props, [token, "%burn%"]) do
      {:ok, [1]} ->
        BalanceStore.burn(account_id, token, amount, timestamp)

      _ ->
        raise IppanError, "Invalid operation"
    end
  end

  def refund(%{account: account, timestamp: timestamp}, hash)
      when byte_size(hash) == 64 do
    hash = Base.decode16!(hash)
    account_id = Map.get(account, :id)
    [sender_id, token, refund_amount] = RefundStore.lookup([hash, account_id, timestamp])

    BalanceStore.send(account_id, sender_id, token, refund_amount, timestamp)
    RefundStore.delete(hash)
  end
end
