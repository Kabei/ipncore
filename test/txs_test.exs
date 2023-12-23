defmodule TransactionsTest do
  use ExUnit.Case
  doctest Ipncore
  import Builder, only: [post: 2]

  test "send txs" do
    # Variables
    vid = 0
    vid1 = 1
    hostname = "visurpay.com"
    hostname1 = "ippan.co.uk"
    {c1, c2} = Builder.test()

    c3 = Builder.Client.new("abcdefghijklmnopqrstuvwxyzabcdef")
    c4 = Builder.Client.new(Base.decode16!("9B85EA08993CA1F9CAFFBD73F7F596D24A7EB419F68DB28AB0EC6A1149483E24"))
    {:ok, c2} = Builder.wallet_new(c2, vid) |> post(hostname)
    {:ok, c3} = Builder.wallet_new(c3, vid1) |> post(hostname1)
    {:ok, c4} = Builder.wallet_new(c4, vid1) |> post(hostname1)

    # First
    # {:ok, c1} = Builder.wallet_new(c1, vid) |> post(hostname)

    # Next
    {:ok, c1} = Builder.coin_new(c1, "IPN", [[c1.id, 1000000], [c2.id, 1000000], [c3.id, 1000000], [c4.id, 1000000]]) |> post(hostname)

    {:ok, c1} = Builder.coin_new(c1, "IPN", [[c2.id, 10000000], [c3.id, 10000000]]) |> post(hostname)


    {:ok, c1} = Builder.coin_new(c1, "IPN", [[c2.id, 1000000]]) |> post(hostname)
    {:ok, c1} = Builder.coin_new(c1, "IPN", [["0xziUv22NNVEPt8ikCGb7xip2CPgB", 1000000]]) |> post(hostname)

    {:ok, c1} = Builder.coin_new(c1, "IPN", [["0x2gosNPWWeLbJfAx5aMnqTuXyYZSv", 400000]]) |> post(hostname)

    # Tokens
    {:ok, c2} = Builder.token_new(c2, "USD", c2.id, "Dollar", 2, "$", 0, %{"avatar" => "https://ippan.com/images/USD.png", "props" => ["coinbase", "lock", "burn", "drop"], "env" => %{}}) |> post(hostname)
    {:ok, c2} = Builder.token_update(c2, "USD", %{"name" => "Dollar"}) |> post(hostname)
    {:ok, c2} = Builder.coin_new(c2, "USD", [[c2.id, 1000000]]) |> post(hostname)

    {:ok, c3} = Builder.token_new(c3, "EUR", c3.id, "Euro", 2, "€", 0, %{"avatar" => "https://ippan.com/images/EUR.png", "props" => ["coinbase", "lock", "burn", "drop"], "env" => %{}}) |> post(hostname1)
    {:ok, c3} = Builder.token_new(c3, "GBP", c3.id, "Pound", 2, "£", 0, %{"avatar" => "https://ippan.com/images/GBP.png", "props" => ["coinbase", "lock", "burn", "drop"], "env" => %{}}) |> post(hostname1)

    {:ok, c1} = Builder.token_update(c1, "XPN", %{"avatar" => "https://ippan.com/images/XPN2.png"}) |> post(hostname)


    # Coins
    {:ok, c1} = Builder.coin_send(c1, c2.id, "IPN", 250) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.id, "USD", 2500) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.id, "IPN", 150, "test IPN", true) |> post(hostname)
    {:ok, c1} = Builder.coin_send(c1, c2.id, "USD", 1500, "test USD", true) |> post(hostname)
    {:ok, c2} = Builder.coin_lock(c2, c1.id, "IPN", 50) |> post(hostname)
    {:ok, c2} = Builder.coin_unlock(c2, c1.id, "IPN", 50) |> post(hostname)
    {:ok, c2} = Builder.coin_send(c2, c3.id, "USD", 150, "test USD") |> post(hostname)

    {:ok, c2} = Builder.coin_send(c2, c3.id, "USD", 500) |> post(hostname)
    {:ok, c2} = Builder.coin_send(c2, c3.id, "USD", 500, "Tx Refund", true) |> post(hostname)
    {:ok, c2} = Builder.coin_refund(c2, "537163378D903B3E1C631B767EF7C33916C920FCCFE54402750F60AFE4855CBD") |> post(hostname)


    {:ok, c2} = Builder.coin_multisend(c2, "USD", [[c1.id, 1000], [c3.id, 2000]], "Multisend") |> post(hostname)

    {:ok, c2} = Builder.domain_new(c2, "example.ipn", c2.id, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_new(c2, "example2.ipn", c2.id, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_new(c2, "example3.ipn", c2.id, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_new(c2, "example4.ipn", c2.id, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_update(c2, "example.ipn", %{"email" => "pop@email.com"}) |> post(hostname)
    {:ok, c2} = Builder.domain_renew(c2, "example.ipn", 1000) |> post(hostname)
    {:ok, c2} = Builder.domain_delete(c2, "example.ipn") |> post(hostname)
    {:ok, c2} = Builder.dns_new(c2, "example.ipn", 1, "192.168.0.1", 600) |> post(hostname)
    {:ok, c2} = Builder.dns_update(c2, "example.ipn", "EA239F6544AC1EB1AE0A9DE3FEDB2BAC", %{"data" => "192.168.0.2"}) |> post(hostname)

    {:ok, c1} = Builder.env_set(c1, "test", "value-test") |> post(hostname)
    {:ok, c1} = Builder.env_delete(c1, "test") |> post(hostname)

    {:ok, c1} = Builder.validator_update(c1, 0, %{"fa" => 2}) |> post(hostname)
    {:ok, c1} = Builder.validator_update(c1, 1, %{"owner" => "0x2gosNPWWeLbJfAx5aMnqTuXyYZSv"}) |> post(hostname)

    # Finish
    {:ok, c2} = Builder.coin_burn(c2, "IPN", 50) |> post(hostname)
    {:ok, c2} = Builder.dns_delete(c2, "example.ipn") |> post(hostname)
    {:ok, c2} = Builder.domain_delete(c2, "example.ipn") |> post(hostname)
    {:ok, c2} = Builder.token_delete(c2, "USD") |> post(hostname)
    {:ok, c1} = Builder.env_delete(c1, "test") |> post(hostname)

    {:ok, c3} = Builder.token_prop_add(c3, "GBP", "erase") |> post(hostname1)
    {:ok, c3} = Builder.token_prop_drop(c3, "GBP", "lock") |> post(hostname1)
    {:ok, c1} = Builder.token_env_put(c1, "XPN", "expiry", 20) |> post(hostname)
    {:ok, c1} = Builder.validator_env_put(c1, 0, "erase", "true") |> post(hostname)
    {:ok, c1} = Builder.validator_env_delete(c1, 0, "erase") |> post(hostname)

    {:ok, c1} = Builder.coin_lock(c1, c2.id, "IPN", 50) |> post(hostname)
    {:ok, c1} = Builder.coin_unlock(c1, c2.id, "IPN", 50) |> post(hostname)

    {:ok, c2} = Builder.coin_drop(c2, "USD", 2500) |> post(hostname)
    {:ok, c2} = Builder.coin_reload(c2, "XPN") |> post(hostname)

    # Other
    # {:ok, c2} = Builder.coin_refund(c1, hash) |> post(hostname)
    # {:ok, c2} = Builder.dns_update(c2, "example.ipn", "a", "192.168.0.1", hash) |> post(hostname)

    c2 = %{c2 | nonce: 312}
    c3 = %{c3 | nonce: 670}


    c2 = Enum.reduce_while(1..1000, c2, fn _, x ->
      monto = 10 + :rand.uniform(200)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_send(x, c4.id, "USD", monto, "random c2 USD") |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)

    c4 = Enum.reduce_while(1..1000, c4, fn _, x ->
      monto = 10 + :rand.uniform(200)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_send(x, c2.id, "EUR", monto, "random c4 EUR") |> post(hostname1) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)

    c2 = Enum.reduce_while(1..50, c2, fn _, x ->
      monto = 10 + :rand.uniform(500)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_send(x, "0xziUv22NNVEPt8ikCGb7xip2CPgB", "USD", monto, "random c2") |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)

    c3 = Enum.reduce_while(1..2000, c3, fn _, x ->
      monto = 100 + :rand.uniform(100)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_send(x, c2.id, "USD", monto, "random c3") |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)

    c2 = Enum.reduce_while(1..1000, c2, fn _, x ->
      monto = 10 + :rand.uniform(500)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_send(x, c3.id, "IPN", monto, "random IPN c2") |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)


    defmodule RandomString do
      def generate do
        :crypto.strong_rand_bytes(8)
        |> Base.encode16(case: :lower)
      end
    end

    c2 = Enum.reduce_while(1..5, c2, fn _, y ->
      dominio = RandomString.generate <> ".ipn"
      IO.puts("#{y.nonce} - DOMINIO: #{dominio}")
      case Builder.domain_new(c2, dominio, c2.id, 2, %{"email" => "name@example.com", "avatar" => "https://avatar.com"}) |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, y}
      end
    end)


    {:ok, c3} = Builder.coin_drop(c3, "USD", 2500) |> post(hostname)
    c3 = Enum.reduce_while(1..100, c3, fn _, x ->
      monto = 10 + :rand.uniform(500)
      IO.puts("#{x.nonce} - monto: #{monto}")
      case Builder.coin_drop(c3, "USD", monto) |> post(hostname) do
        {:ok, x} -> {:cont, x}
        {:error, _status, msg} ->
          IO.inspect(msg)
          {:halt, x}
      end
    end)


  end
end
