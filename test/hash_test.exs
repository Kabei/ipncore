defmodule HashTest do
  use ExUnit.Case
  doctest Ipncore

  test "hashes" do
    data = :crypto.strong_rand_bytes(1024)

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
      "xxhash" => fn ->
        XXHash.xxh64(data, 2_148_456_111)
      end
    })
  end
end
