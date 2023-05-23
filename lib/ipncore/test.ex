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

    _sk2 =
      <<140, 176, 158, 128, 218, 167, 112, 93, 41, 250, 55, 168, 169, 1, 96, 21, 68, 114, 250,
        100, 126, 90, 183, 50, 86, 23, 97, 61, 25, 114, 63, 84>>

    {pk, address} = Test.gen_secp256k1(sk)
    # {pk2, address2} = Test.gen_secp256k1(sk2)
    # IO.inspect(byte_size(pk))
    # IO.inspect(address)
    {pkv, _skv, addressv} = Test.gen_falcon(seed)

    Test.wallet_new(pk, 0)
    |> run()

    Test.token_new(sk, address, "IPN", address, "IPPAN", 9, "Þ", %{
      "avatar" => "https://avatar.com",
      "props" => ["coinbase", "lock", "burn"]
    })
    |> run()

    Test.validator_new(sk, address, 0, addressv, "ippan.uk", "main-core", pkv, 1, 0.01)
    |> run()
  end

  def build_request({body, sig}) do
    IO.puts(body)
    IO.puts(Fast64.encode64(sig))
  end

  def build_request(body) do
    IO.puts(body)
  end

  def run({body, sig}) do
    hash = Blake3.hash(body)
    size = byte_size(body) + byte_size(sig)
    fsig = sig
    # IO.inspect(fsig)
    RequestHandler.handle(hash, body, size, fsig)
  end

  def run(body) do
    hash = Blake3.hash(body)
    size = byte_size(body)
    RequestHandler.handle(hash, body, size)
  end

  # {pk, address} = Test.gen_secp256k1(sk)
  # {pk2, address2} = Test.gen_secp256k1(sk2)
  def gen_secp256k1(sk) do
    pk =
      sk
      |> ExSecp256k1.Impl.create_public_key()
      |> elem(1)
      |> ExSecp256k1.Impl.public_key_compress()
      |> elem(1)

    {pk, Address.hash(0, pk)}
  end

  # {pkv, skv, seedv, addressv} = Test.gen_falcon()
  def gen_falcon(seed) do
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    {pk, sk, Address.hash(1, pk)}
  end

  # Test.wallet_new(pk, 0) |> Test.build_request
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

  # Test.env_set(sk, address, "test", "value-test") |> Test.build_request
  def env_set(secret, address, name, value) do
    body =
      [50, :os.system_time(:millisecond), address, [name, value]]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.env_delete(sk, address, "test") |> Test.build_request
  def env_delete(secret, address, name) do
    body =
      [51, :os.system_time(:millisecond), address, [name]]
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
    # IO.inspect(sig)
    {body, sig}
  end

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
      )
      when is_float(fee) and fee_type in 0..2 do
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

  # Test.tx_coinbase(sk, address, "IPN", [[address2, 50000000]]) |> Test.build_request()
  def tx_coinbase(secret, address, token, outputs) do
    body =
      [300, :os.system_time(:millisecond), address, [token, outputs]]
      |> Jason.encode!()

    hash = hash_fun(body)

    sig = signature64(address, secret, hash)

    {body, sig}
  end

  # Test.tx_send(sk2, address2, address, "IPN", 50000) |> Test.build_request()
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
    # IO.inspect(first)
    s =
      first <>
        case first do
          "0" ->
            ExSecp256k1.Impl.sign_compact(msg, secret)
            |> elem(1)
            |> elem(0)

          "1" ->
            Falcon.sign(secret, msg)
            |> elem(1)
        end

    #  IO.inspect(s)

    s
    # |> Fast64.encode64()
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
