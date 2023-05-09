defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  require Logger
  @json Application.compile_env(:ipncore, :json)

  alias Ippan.RequestHandler
  # alias Ippan.Events
  # alias Ippan.Address

  defp new_hash_signature(auth_client, txhash, pkhash2) do
    secret = :rand.bytes(32)
    pk = Blake3.hash(secret)

    new_mac = :crypto.mac(:poly1305, next_sk, txhash)

    new_hmac=
      new_mac
    |> Blake3.hash()
    |> :binary.part(16, 16)

    {:ok, pkhash2 <> pk <> mac <> txhash, new_hmac}
  end

  test "create account" do
    seed = :rand.bytes(48)
    {:ok, pk, sk} = Falcon.gen_keys_from_seed(seed)

    # hash entangled
    skh = :rand.bytes(32)
    skh2 = :rand.bytes(32)

    pkhash = Blake3.hash(skh)
    pkhash2 = Blake3.hash(skh2)

    hmac =
      :crypto.mac(:poly1305, skh, "")
      |> Blake3.hash()
      |> :binary.part(16, 16)

    account_id = "kambei"
    validator_id = 0
    timestamp = :os.system_time(:millisecond)

    args = [
      validator_id,
      Base.encode16(pk),
      Base.encode16(pkhash),
      Base.encode16(pkhash2),
      Base.encode16(hmac)
    ]

    hash = RequestHandler.compute_hash(0, timestamp, account_id, args)

    {:ok, signature} = Falcon.sign(sk, hash)
    request = [0, timestamp, account_id, args, Base.encode64(signature)]

    @json.encode!(request)
    |> IO.puts()

    IO.puts("Falcon seed")

    Base.encode64(seed)
    |> IO.puts()

    IO.puts("Secret Hash")

    Base.encode64(skh)
    |> IO.puts()

    IO.puts("Secret Hash 2")

    Base.encode64(skh2)
    |> IO.puts()

    # start_time = :erlang.monotonic_time(:nanosecond)
    # RequestHandler.handle(request)
    # end_time = :erlang.monotonic_time()
    # duration = end_time - start_time
    # IO.puts("Duration: #{duration} ns")

    # Benchee.run(
    #   %{
    #     "account.new" => fn ->
    #       RequestHandler.handle(request)
    #     end
    #   },
    #   time: 5,
    #   warmup: 5,
    #   memory_time: 5
    # )
  end

  test "create validator" do
  end

  # start_time = :erlang.monotonic_time(:millisecond)
  #   1..10
  #   |> Task.async_stream(fn _ ->
  #     for _ <- 1..100_000 do
  #       RequestHandler.handle(request)
  #     end
  #   end, timeout: :infinity)
  #   |> Enum.to_list()

  #   end_time = :erlang.monotonic_time(:millisecond)
  #   duration = end_time - start_time
  #   IO.puts("Duration: #{duration} ms")
end
