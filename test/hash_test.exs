defmodule HashTest do
  use ExUnit.Case
  doctest Ipncore

  test "hashes" do
    data = :crypto.strong_rand_bytes(256)

    Benchee.run(%{
      "blake3" => fn ->
        Blake3.hash(data)
      end,
      "sha256" => fn ->
        :crypto.hash(:sha256, data)
      end,
      "sha3_256" => fn ->
        :crypto.hash(:sha3_256, data)
      end,
      "blake2b" => fn ->
        :crypto.hash(:blake2b, data)
      end,
      "phash" => fn ->
        :erlang.phash(data, 1000)
      end,
      "phash2" => fn ->
        :erlang.phash2(data)
      end,
      "phash2-range" => fn ->
        :erlang.phash2(data, 1000)
      end
    })
  end

  fun = fn ->
    rem(:erlang.phash2(data), 8)
  end

  :timer.tc(fun)
end
