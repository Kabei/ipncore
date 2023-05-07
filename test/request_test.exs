defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  require Logger

  alias Ippan.RequestHandler
  # alias Ippan.Events
  # alias Ippan.Address

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
    timestamp = :os.system_time(1000)
    args = [validator_id, pk, pkhash, pkhash2, hmac]
    hash = RequestHandler.compute_hash(0, timestamp, account_id, args)

    signature = Falcon.sign(sk, hash)
    request = {0, timestamp, account_id, args, signature}

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

  # test "token.new" do
  # end

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
