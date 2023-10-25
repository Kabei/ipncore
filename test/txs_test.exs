defmodule TransactionsTest do
  use ExUnit.Case
  doctest Ipncore
  import Builder, only: [post: 2]

  test "send txs" do
    hostname = "visurpay.com"
    {c1, c2} = Builder.test()
    {:ok, c2} = Builder.wallet_sub(c2, vid) |> post(hostname)

    {:ok, c2} = Builder.coin_send(c1, c2.address, "IPN", 50) |> post(hostname)
  end
end
