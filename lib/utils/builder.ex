defmodule Builder do
  alias Ippan.Address
  require Logger

  # {pk, sk, pk2, sk2, address, address2} = Builder.test()
  def test do
    sk =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    sk2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {pk, sk, address} = Builder.gen_ed25519(sk)
    {pk2, sk2, address2} = Builder.gen_ed25519(sk2)
    {pk, sk, pk2, sk2, address, address2}
  end

  # {fpk1, fskf1, faddress1, fpk2, fsk2, faddress2} = Builder.test_falcon()
  def test_falcon do
    seed1 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    seed2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {pk1, sk1, address1} = Builder.gen_falcon(seed1)
    {pk2, sk2, address2} = Builder.gen_falcon(seed2)

    {pk1, sk1, address1, pk2, sk2, address2}
  end

  def build_request({body, sig}) do
    IO.puts(body)
    IO.puts(sig)
  end

  def build_request(body) do
    IO.puts(body)
  end

  # {pk, sk, address} = Builder.gen_ed25519()
  def gen_ed25519 do
    {:ok, {pk, sk}} =
      :rand.bytes(32)
      |> Cafezinho.Impl.keypair_from_seed()

    {pk, sk, Address.hash(0, pk)}
  end

  # {pk, sk, address} = Builder.gen_ed25519(seed)
  def gen_ed25519(seed) do
    {:ok, {pk, sk}} = Cafezinho.Impl.keypair_from_seed(seed)

    {pk, sk, Address.hash(0, pk)}
  end

  # {pkv, skv, addressv} = Builder.gen_falcon(seed)
  def gen_falcon(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, Address.hash(1, pk)}
  end

  # Builder.wallet_sub(sk, address, pk, 0, 0) |> Builder.build_request
  def wallet_sub(secret, address, pk, validator_id, sig_type) do
    body =
      [0, :os.system_time(:millisecond), address, Fast64.encode64(pk), validator_id, sig_type]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.wallet_unsub(sk, address) |> Builder.build_request
  def wallet_unsub(secret, address) do
    body =
      [1, :os.system_time(:millisecond), address]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.env_set(sk, address, "test", "value-test") |> Builder.build_request
  def env_set(secret, address, name, value) do
    body =
      [50, :os.system_time(:millisecond), address, name, value]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.env_delete(sk, address, "test") |> Builder.build_request
  def env_delete(secret, address, name) do
    body =
      [51, :os.system_time(:millisecond), address, name]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.validator_new(sk, address, "ippan.net", 5815, address, "net core", pkv, 1, 5.0, %{"avatar" => "https://avatar.com"}) |> Builder.build_request()
  def validator_new(
        secret,
        address,
        hostname,
        port,
        owner,
        name,
        pubkey,
        net_pubkey,
        fee_type,
        fee,
        opts \\ %{}
      )
      when is_float(fee) and fee_type in 0..2 do
    body =
      [
        100,
        :os.system_time(:millisecond),
        address,
        hostname,
        port,
        owner,
        name,
        Fast64.encode64(pubkey),
        Fast64.encode64(net_pubkey),
        fee_type,
        fee,
        opts
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.validator_update(sk, address, 1, %{"fee" => 7.0}) |> Builder.build_request()
  def validator_update(secret, address, id, params) do
    body =
      [101, :os.system_time(:millisecond), address, id, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.validator_delete(sk, address, 1) |> Builder.build_request()
  def validator_delete(secret, address, id) do
    body =
      [102, :os.system_time(:millisecond), address, id]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", 0, %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]})
  # Builder.token_new(sk, address, "USD", address, "DOLLAR", 5, "$", 0, %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]}) |> Builder.build_request
  def token_new(
        secret,
        address,
        token_id,
        owner,
        name,
        decimal,
        symbol,
        max_supply,
        %{
          "avatar" => _avatar_url,
          "props" => _props
        } = opts
      ) do
    body =
      [
        200,
        :os.system_time(:millisecond),
        address,
        token_id,
        owner,
        name,
        decimal,
        symbol,
        max_supply,
        opts
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)
    # IO.inspect(sig)
    {body, sig}
  end

  # Builder.token_update(sk, address, "USD", %{"name" => "Dollar"}) |> Builder.build_request()
  def token_update(secret, address, id, params) do
    body =
      [201, :os.system_time(:millisecond), address, id, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.token_delete(sk, address, "USD") |> Builder.build_request()
  def token_delete(secret, address, id) do
    body =
      [202, :os.system_time(:millisecond), address, id]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def balance_lock(secret, address, to, token_id, amount) do
    body =
      [250, :os.system_time(:millisecond), address, to, token_id, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def balance_unlock(secret, address, to, token_id, amount) do
    body =
      [251, :os.system_time(:millisecond), address, to, token_id, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.tx_coinbase(sk, address, "IPN", [[address2, 50000000]]) |> Builder.build_request()
  def tx_coinbase(secret, address, token, outputs) do
    body =
      [300, :os.system_time(:millisecond), address, token, outputs]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.tx_send(sk2, address2, address, "IPN", 50000) |> Builder.build_request()
  # Builder.tx_send(sk2, address2, address, "IPN", 4000) |> Builder.build_request()
  def tx_send(secret, address, to, token, amount) do
    body =
      [301, :os.system_time(:millisecond), address, to, token, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def tx_send(secret, address, to, token, amount, note) do
    body =
      [301, :os.system_time(:millisecond), address, to, token, amount, note]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def tx_refundable(secret, address, to, token, amount) do
    body =
      [302, :os.system_time(:millisecond), address, to, token, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.tx_refund(sk, address, "21520DCFF38E79472E768E98A0FEFC901F4AADA2633E23E116E74181651290BA") |> Builder.build_request()
  def tx_refund(secret, address, hash) do
    body =
      [303, :os.system_time(:millisecond), address, hash]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.tx_burn(sk2, address2, "IPN", 1000) |> Builder.build_request()
  def tx_burn(secret, address, token, amount) do
    body =
      [304, :os.system_time(:millisecond), address, token, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.domain_new(sk2, address2, "example.ipn", address, 2, %{"email" => "asd@example.com", "avatar" => "https://avatar.com"}) |> Builder.build_request()
  def domain_new(
        secret,
        address,
        domain_name,
        owner,
        days,
        %{
          "email" => _email,
          "avatar" => _avatar
        } = params
      ) do
    body =
      [400, :os.system_time(:millisecond), address, domain_name, owner, days, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.domain_update(sk, address, "example.ipn", %{"email" => "pop@email.com"}) |> Builder.build_request()
  def domain_update(
        secret,
        address,
        domain_name,
        params
      ) do
    body =
      [401, :os.system_time(:millisecond), address, domain_name, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.domain_delete(sk, address, "example.ipn") |> Builder.build_request()
  def domain_delete(
        secret,
        address,
        domain_name
      ) do
    body =
      [402, :os.system_time(:millisecond), address, domain_name]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Builder.domain_renew(sk, address, "example.ipn", 1000) |> Builder.build_request()
  def domain_renew(
        secret,
        address,
        domain_name,
        days
      ) do
    body =
      [403, :os.system_time(:millisecond), address, domain_name, days]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def dns_new(secret, address, fullname, type, data, ttl) do
    body =
      [500, :os.system_time(:millisecond), address, fullname, type, data, ttl]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def dns_update(secret, address, fullname, dns_hash16, params) do
    body =
      [501, :os.system_time(:millisecond), address, fullname, dns_hash16, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def dns_delete(secret, address, fullname) do
    body =
      [502, :os.system_time(:millisecond), address, fullname]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def dns_delete(secret, address, fullname, type) when is_integer(type) do
    body =
      [502, :os.system_time(:millisecond), address, fullname, type]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def dns_delete(secret, address, fullname, hash16) do
    body =
      [502, :os.system_time(:millisecond), address, fullname, hash16]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  def custom(secret, address, type, timestamp, args) do
    {:ok, body} =
      [type, timestamp, address, args]
      |> Jason.encode()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  defp signature64(address, secret, msg) do
    [type_sig, _] = String.split(address, "x", parts: 2)

    result =
      case type_sig do
        "0" ->
          Cafezinho.Impl.sign(msg, secret)
          |> elem(1)

        "1" ->
          Falcon.sign(secret, msg)
          |> elem(1)
      end

    Fast64.encode64(result)
  end

  defp hash_fun(msg) do
    Blake3.hash(msg)
  end
end
