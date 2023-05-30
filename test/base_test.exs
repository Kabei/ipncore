defmodule BaseTest do
  use ExUnit.Case
  doctest Ipncore

  test "base" do
    data16 = :crypto.strong_rand_bytes(32) |> Base.encode16()
    data64 = :crypto.strong_rand_bytes(32) |> Base.encode64()

    Benchee.run(%{
      "base16" => fn ->
        Base.decode16!(data16)
      end,
      "base64" => fn ->
        Base.decode64!(data64)
      end,
      "fast64" => fn ->
        Fast64.decode64(data64)
      end
    })
  end
end
