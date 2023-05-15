defmodule SqliteTest do
  use ExUnit.Case
  doctest Ipncore

  test "sqlite3" do
    # {:ok, pid} = BalanceStore.start_link("balance.db")
    # %{conn: conn} = BalanceStore.get_state()
    address = :crypto.strong_rand_bytes(42)
    address2 = :crypto.strong_rand_bytes(42)
    token = :crypto.strong_rand_bytes(3)
    time = :os.system_time(1000)

    BalanceStore.insert_sync([address, token, 5_000_000_000_000, 0, 0, 0, time, time])
    BalanceStore.insert([address2, token, 20_000_000, 0, 0, 0, time, time])
    # BalanceStore.all()

    Benchee.run(%{
      "balance" => fn ->
        BalanceStore.send(address, address2, token, 937, time)
      end
    }) != :ok
  end
end
