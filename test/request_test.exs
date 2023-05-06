defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  require Logger

  alias Ippan.Events
  alias Ippan.RequestHandler
  alias Ippan.Address

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
    timestamp = :os.system_time(1000)
    args = [pk, pkhash, pkhash2, hmac]
    hash = RequestHandler.compute_hash(0, timestamp, account_id, args)

    signature = Falcon.sign(sk, hash)
    request = {0, timestamp, account_id, args, signature}
    # result = RequestHandler.handle(request)
    # Logger.info(inspect(result))

    Benchee.run(%{
      "account.new" => fn ->
        RequestHandler.handle(request)
      end
    })
  end

  # test "token.new" do
  # end
end
