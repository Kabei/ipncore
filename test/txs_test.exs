defmodule TransactionsTest do
  use ExUnit.Case
  doctest Ipncore
  import Builder, only: [post: 2]

  test "send txs" do
    # Variables
    hostname = "visurpay.com"
    {c1, c2} = Builder.test()

    # First
    {:ok, c1} = Builder.wallet_new(c1, vid) |> post(hostname)
    {:ok, c2} = Builder.wallet_new(c2, vid) |> post(hostname)

    # Next
    {:ok, c2} = Builder.coin_coinbase(c2, "IPN", [[c1.adress, 50000000]]) |> post(hostname)
    {:ok, c2} = Builder.token_new(c2, "USD", c2.address, "DOLLAR", 2, "$", 0, %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]}) |> post(hostname)
    {:ok, c2} = Builder.token_update(c2, "USD", %{"name" => "Dollar"}) |> post(hostname)
    {:ok, c2} = Builder.coin_coinbase(c2, "USD", [[c1.adress, 50000]]) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.address, "IPN", 250) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.address, "USD", 2500) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.address, "IPN", 150, "test IPN", true) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.address, "USD", 1500, "test USD", true) |> post(hostname)
    {:ok, c2} = Builder.coin_lock(c2, c1.address, "IPN", 50) |> post(hostname)
    {:ok, c2} = Builder.coin_unlock(c2, c1.address, "IPN", 50) |> post(hostname)

    {:ok, c2} = Builder.domain_new(c2, "example.ipn", c2.address, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_update(c2, "example.ipn", %{"email" => "pop@email.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_renew(c2, "example.ipn", 1000) |> post(hostname)
    {:ok, c2} = Builder.dns_new(c2, "example.ipn", "a", "192.168.0.1", 600) |> post(hostname)

    {:ok, c1} = Builder.env_set(c1, "test", "value-test") |> post(hostname)

    # Finish
    {:ok, c2} = Builder.coin_burn(c2, "IPN", 50) |> post(hostname)
    {:ok, c2} = Builder.dns_delete(c2, "example.ipn") |> post(hostname)
    {:ok, c2} = Builder.domain_delete(c2, "example.ipn") |> post(hostname)
    {:ok, c2} = Builder.token_delete(c2, "USD") |> post(hostname)
    {:ok, c1} = Builder.env_delete(c1, "test") |> post(hostname)

    # Other
    # {:ok, c2} = Builder.coin_refund(c1, hash) |> post(hostname)
    # {:ok, c2} = Builder.dns_update(c2, "example.ipn", "a", "192.168.0.1", hash) |> post(hostname)




  end
end
