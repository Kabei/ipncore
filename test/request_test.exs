defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  @json Application.compile_env(:ipncore, :json)

  alias Ippan.RequestHandler
  # alias Ippan.Events
  # alias Ippan.Address

  def default_hash(data) do
    Blake3.hash(data)
  end

  test "create account" do
    seed = :rand.bytes(48)
    {:ok, pk, _sk} = Falcon.gen_keys_from_seed(seed)

    # account_id = "kambei"
    validator_id = 0
    timestamp = :os.system_time(:millisecond)

    args = [
      validator_id,
      Fast64.encode64(pk)
    ]

    IO.puts("Message")

    message =
      [0, timestamp, args]
      |> @json.encode!()

    hash = default_hash(message)

    IO.puts(message)

    IO.puts("Hash")
    # IO.puts(Fast64.encode16(hash))
    IO.puts(Base.encode16(hash))
    IO.puts("Size: #{byte_size(message)}")

    IO.puts("Falcon seed")

    Fast64.encode64(seed)
    |> IO.puts()
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
