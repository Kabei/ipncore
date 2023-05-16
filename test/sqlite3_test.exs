defmodule SqliteTest do
  use ExUnit.Case
  doctest Ipncore

  test "sqlite3" do
    # {:ok, pid} = BalanceStore.start_link("balance.db")
    # %{conn: conn} = BalanceStore.get_state()
    address = :crypto.strong_rand_bytes(36)
    address2 = :crypto.strong_rand_bytes(36)
    token = :crypto.strong_rand_bytes(3)
    time = :os.system_time(1000)

    BalanceStore.insert_sync([address, token, 5_000_000_000_000, 0, 0, 0, time, time])
    BalanceStore.insert_sync([address2, token, 20_000, 0, 0, 0, time, time])
    # BalanceStore.all()

    start_time = :os.system_time(:microsecond)

    0..7
    |> Task.async_stream(
      fn _ ->
        iter_insert(100_000, address, address2, token, time)
        # Enum.each(1..500_000, fn _ ->
        #   BalanceStore.send(address, address2, token, 500, time)
        # end)
      end,
      timeout: :infinity
    )
    |> Enum.to_list()

    end_time = :os.system_time(:microsecond)
    IO.puts("Time: #{end_time - start_time} µs")

    # Benchee.run(%{
    #   "balance" => fn ->
    #     BalanceStore.send(address, address2, token, 937, time)
    #   end
    # })
  end

  defp iter_insert(1, _, _, _, _), do: :ok

  defp iter_insert(n, address, address2, token, time) do
    BalanceStore.send(address, address2, token, 500, time)
    iter_insert(n - 1, address, address2, token, time)
  end
end
