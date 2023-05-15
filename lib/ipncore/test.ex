defmodule Test do
  alias Ippan.Address

  # {pkv, skv, seedv, addressv} = Test.gen_falcon()
  def gen_falcon do
    seed = :crypto.strong_rand_bytes(48)

    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, seed, Address.hash(0, pk)}
  end

  # {pk, sk, address} = Test.gen_secp256k1()
  # {pk2, sk2, address2} = Test.gen_secp256k1()
  def gen_secp256k1 do
    sk = :crypto.strong_rand_bytes(32)

    pk =
      sk
      |> ExSecp256k1.Impl.create_public_key()
      |> elem(1)
      |> ExSecp256k1.Impl.public_key_compress()
      |> elem(1)

    {pk, sk, Address.hash(1, pk)}
  end

  # Test.wallet_new(pk, 0) |> IO.puts
  def wallet_new(pk, validator_id) do
    body =
      [0, :os.system_time(:millisecond), [validator_id, Fast64.encode64(pk)]]
      |> Jason.encode!()

    body
  end

  # Test.wallet_subscribe(sk, address, 1)
  def wallet_subscribe(secret, address, validator_id) do
    body =
      [1, :os.system_time(:millisecond), address, [validator_id]]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]})
  def token_new(
        secret,
        address,
        token_id,
        owner,
        name,
        decimal,
        symbol,
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
        [token_id, owner, name, decimal, symbol, opts]
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # {} = Test.gen_falcon()
  # Test.validator_new(sk, address, 0, address, "ippan.uk", "main core", pkv, 1, 5)
  def validator_new(
        secret,
        address,
        id,
        owner,
        hostname,
        name,
        pubkey,
        fee_type,
        fee
      ) do
    body =
      [
        100,
        :os.system_time(:millisecond),
        address,
        [
          id,
          owner,
          hostname,
          name,
          Fast64.encode64(pubkey),
          fee_type,
          fee
        ]
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_coinbase(sk, address, "IPN", [address2, 50000000])
  def tx_coinbase(secret, address, token, outputs) do
    body =
      [300, :os.system_time(:millisecond), address, [token, outputs]]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_send(sk, address, address2, "IPN", 50000)
  def tx_send(secret, address, to, token, amount) do
    body =
      [301, :os.system_time(:millisecond), address, [to, token, amount]]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  defp signature64(address, secret, msg) do
    <<first::bytes-size(1), _rest::binary>> = address

    case first do
      "0" ->
        Falcon.sign(secret, msg)

      "1" ->
        ExSecp256k1.Impl.sign(msg, secret)

      true ->
        throw("address type not supported")
    end
    |> Fast64.encode64()
  end

  defp hash_fun(msg) do
    Blake3.hash(msg)
  end
end
