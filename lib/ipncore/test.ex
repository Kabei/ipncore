defmodule Test do
  alias Ippan.Address
  alias Ippan.RequestHandler

  def init do
    sk =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    seed =
      <<114, 126, 255, 205, 14, 72, 7, 127, 21, 47, 45, 57, 188, 66, 144, 114, 118, 204, 255, 86,
        236, 6, 168, 77, 247, 60, 145, 142, 137, 32, 81, 188, 167, 95, 239, 138, 212, 128, 12,
        211, 239, 154, 118, 40, 154, 90, 156, 28>>

    {pk, address} = Test.gen_secp256k1(sk)
    # {pk2, address2} = Test.gen_secp256k1(sk2)
    # IO.inspect(byte_size(pk))
    # IO.inspect(address)
    {pkv, _skv, addressv} = Test.gen_falcon(seed)

    Test.wallet_new(pk, 0)
    |> run()

    Test.wallet_new(pkv, 1) |> run()

    Test.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", %{
      "avatar" => "https://avatar.com",
      "props" => ["coinbase", "lock", "burn"]
    })
    |> run()

    Test.validator_new(sk, address, 0, addressv, "ippan.uk", "main-core", pkv, 1, 0.01)
    |> run()
  end

  # {pk, sk, pk2, sk2, address, address2} = Test.test()
  def test do
    sk =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    sk2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {pk, address} = Test.gen_secp256k1(sk)
    {pk2, address2} = Test.gen_secp256k1(sk2)
    {pk, sk, pk2, sk2, address, address2}
  end

  def test_falcon do
    seed1 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 83>>

    seed2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {pk1, sk1, address1} = Test.gen_falcon(seed1)
    {pk2, sk2, address2} = Test.gen_falcon(seed2)

    {pk1, sk1, address1, pk2, sk2, address2}
  end

  # Test.bench_send(10_000)
  @doc """
  Test.init()
  {pk, sk, pk1, sk1, address, address1} = Test.test()
  Test.wallet_new(pk1, 0) |> Test.run()
  Test.tx_coinbase(sk, address, "IPN", [[address1, 50000000000]]) |> Test.run()

  # falcon
  {pk2, sk2, address2, pk3, sk3, address3} = Test.test_falcon()
  Test.wallet_new(pk2, 0) |> Test.run()
  Test.wallet_new(pk3, 0) |> Test.run()
  Test.tx_coinbase(sk, address, "IPN", [[address2, 50000000000]]) |> Test.run()

  BlockBuilderWork.sync_all()
  """
  require Logger

  @spec bench_send(integer()) :: no_return()
  def bench_send(n, cpus \\ :erlang.system_info(:schedulers_online)) do
    spawn(fn ->
      {_pk, _sk, _pk2, sk1, address, address1} = Test.test()

      chunks = div(n, cpus)

      list =
        for _ <- 1..n do
          Test.tx_send(sk1, address1, address, "IPN", 10 + :rand.uniform(10000))
        end
        |> Enum.chunk_every(chunks)

      # tstream =
      Enum.each(list, fn data ->
        spawn(fn ->
          start_time = :os.system_time(:microsecond)
          run_list(data)
          end_time = :os.system_time(:microsecond)
          IO.puts("Time elapsed: #{end_time - start_time} µs - #{length(data)}")
        end)
      end)
    end)

    # Enum.to_list(tstream)

    # end_time = :os.system_time(:microsecond)

    # IO.puts("Time elapsed: #{end_time - start_time} µs")
  end

  # Test.fbench_send(10_000)
  def fbench_send(n, cpus \\ :erlang.system_info(:schedulers_online)) do
    spawn(fn ->
      {_pk2, sk2, address2, _pk3, _sk3, address3} = Test.test_falcon()

      chunks = div(n, cpus)

      list =
        for _ <- 1..n do
          Test.tx_send(sk2, address2, address3, "IPN", 10 + :rand.uniform(50000))
        end
        |> Enum.chunk_every(chunks)

      # tstream =
      Enum.each(list, fn data ->
        spawn(fn ->
          start_time = :os.system_time(:microsecond)
          run_list(data)
          end_time = :os.system_time(:microsecond)
          IO.puts("Time elapsed: #{end_time - start_time} µs - #{length(data)}")
        end)
      end)
    end)
  end

  def build_request({body, sig}) do
    IO.puts(body)
    IO.puts(Fast64.encode64(sig))
  end

  def build_request(body) do
    IO.puts(body)
  end

  def run_list([]), do: :ok

  def run_list([first | rest]) do
    run(first)
    run_list(rest)
  end

  def run({body, sig}) do
    try do
      hash = Blake3.hash(body)
      sig = Fast64.decode64(sig)
      size = byte_size(body) + byte_size(sig)
      # RequestHandler.handle_import!(hash, body, size)
      RequestHandler.handle!(hash, body, size, sig)
    rescue
      e ->
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
    end
  end

  def run(body) do
    try do
      hash = Blake3.hash(body)
      size = byte_size(body)
      RequestHandler.handle!(hash, body, size)
    rescue
      e ->
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
    end
  end

  # {pk, address} = Test.gen_secp256k1(sk)
  # {pk2, address2} = Test.gen_secp256k1(sk2)
  def gen_secp256k1(sk) do
    pk =
      sk
      |> ExSecp256k1.Impl.create_public_key()
      |> elem(1)

    # |> ExSecp256k1.Impl.public_key_compress()
    # |> elem(1)

    {pk, Address.hash(0, pk)}
  end

  # {pkv, skv, addressv} = Test.gen_falcon(seed)
  def gen_falcon(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, Address.hash(1, pk)}
  end

  # Test.wallet_new(pk, 0) |> Test.build_request
  def wallet_new(pk, validator_id) do
    body =
      [0, :os.system_time(:millisecond), validator_id, Fast64.encode64(pk)]
      |> Jason.encode!()

    body
  end

  # Test.wallet_subscribe(sk, address, 1) |> Test.build_request
  def wallet_subscribe(secret, address, validator_id) do
    body =
      [1, :os.system_time(:millisecond), address, validator_id]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.env_set(sk, address, "test", "value-test") |> Test.build_request
  def env_set(secret, address, name, value) do
    body =
      [50, :os.system_time(:millisecond), address, name, value]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.env_delete(sk, address, "test") |> Test.build_request
  def env_delete(secret, address, name) do
    body =
      [51, :os.system_time(:millisecond), address, name]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]})
  # Test.token_new(sk, address, "USD", address, "DOLLAR", 5, "$", %{"avatar" => "https://avatar.com", "props" => ["coinbase", "lock", "burn"]}) |> Test.build_request
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
        token_id,
        owner,
        name,
        decimal,
        symbol,
        opts
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)
    # IO.inspect(sig)
    {body, sig}
  end

  # Test.token_update(sk, address, "USD", %{"name" => "Dollar"}) |> Test.build_request()
  def token_update(secret, address, id, params) do
    body =
      [201, :os.system_time(:millisecond), address, id, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.token_delete(sk, address, "USD") |> Test.build_request()
  def token_delete(secret, address, id) do
    body =
      [202, :os.system_time(:millisecond), address, id]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.validator_new(sk, address, 0, address, "ippan.net", "net core", pkv, 1, 5.0) |> Test.build_request()
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
      )
      when is_float(fee) and fee_type in 0..2 do
    body =
      [
        100,
        :os.system_time(:millisecond),
        address,
        id,
        owner,
        hostname,
        name,
        Fast64.encode64(pubkey),
        fee_type,
        fee
      ]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.validator_update(sk, address, 1, %{"fee" => 7.0}) |> Test.build_request()
  def validator_update(secret, address, id, params) do
    body =
      [101, :os.system_time(:millisecond), address, id, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.validator_delete(sk, address, 1) |> Test.build_request()
  def validator_delete(secret, address, id) do
    body =
      [102, :os.system_time(:millisecond), address, id]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_coinbase(sk, address, "IPN", [[address2, 50000000]]) |> Test.build_request()
  def tx_coinbase(secret, address, token, outputs) do
    body =
      [300, :os.system_time(:millisecond), address, token, outputs]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_send(sk2, address2, address, "IPN", 50000) |> Test.build_request()
  # Test.tx_send(sk2, address2, address, "IPN", 4000) |> Test.build_request()
  def tx_send(secret, address, to, token, amount) do
    body =
      [301, :os.system_time(:millisecond), address, to, token, amount]
      |> Jason.encode!()

    # |> :erlang.term_to_binary()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_burn(sk2, address2, "IPN", 1000) |> Test.build_request()
  def tx_burn(secret, address, token, amount) do
    body =
      [302, :os.system_time(:millisecond), address, token, amount]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_refund(sk, address, "21520DCFF38E79472E768E98A0FEFC901F4AADA2633E23E116E74181651290BA") |> Test.build_request()
  def tx_refund(secret, address, hash) do
    body =
      [303, :os.system_time(:millisecond), address, hash]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.domain_new(sk2, address2, "example.ipn", address, 2, %{"email" => "asd@example.com", "avatar" => "https://avatar.com"}) |> Test.build_request()
  def domain_new(
        secret,
        address,
        domain_name,
        owner,
        years,
        %{
          "email" => _email,
          "avatar" => _avatar
        } = params
      ) do
    body =
      [400, :os.system_time(:millisecond), address, domain_name, owner, years, params]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.domain_update(sk, address, "example.ipn", %{"email" => "pop@email.com"}) |> Test.build_request()
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

  # Test.domain_delete(sk, address, "example.ipn") |> Test.build_request()
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

  # Test.domain_renew(sk, address, "example.ipn", 1000) |> Test.build_request()
  def domain_renew(
        secret,
        address,
        domain_name,
        days
      ) do
    body =
      [402, :os.system_time(:millisecond), address, domain_name, days]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  defp signature64(address, secret, msg) do
    <<first::bytes-size(1), _rest::binary>> = address

    (first <>
       case first do
         "0" ->
           ExSecp256k1.Impl.sign_compact(msg, secret)
           |> elem(1)
           |> elem(0)

         "1" ->
           Falcon.sign(secret, msg)
           |> elem(1)
       end)
    |> Fast64.encode64()
  end

  defp hash_fun(msg) do
    Blake3.hash(msg)
  end
end

# hash file blake3 with rust example
# use std::fs::File;
# use std::io::{BufReader, Read};
# use blake3::Hasher;

# fn hash_file(path: &str) -> String {
#     let file = File::open(path).expect("Unable to open file");
#     let mut reader = BufReader::new(file);
#     let mut hasher = Hasher::new();
#     let mut buffer = [0; 1024];
#     loop {
#         let count = reader.read(&mut buffer).expect("Unable to read data");
#         if count == 0 {
#             break;
#         }
#         hasher.update(&buffer[..count]);
#     }
#     let hash = hasher.finalize();
#     format!("{}", hash)
# }
