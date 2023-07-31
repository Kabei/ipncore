defmodule Ippan.P2P do
  alias Phoenix.PubSub

  @seconds <<0>>
  @iv_bytes 12
  @tag_bytes 16
  @pubsub_server :network

  def version, do: <<0::16>>
  def handshake_timeout, do: 5_000

  def decode!(
        <<iv::bytes-size(@iv_bytes), tag::bytes-size(@tag_bytes), ciphertext::binary>>,
        sharedkey
      ) do
    :crypto.crypto_one_time_aead(
      :chacha20_poly1305,
      sharedkey,
      iv,
      ciphertext,
      @seconds,
      tag,
      false
    )
    |> :msgpack.unpack()
    |> elem(1)
  end

  def decode!(packet, _sharedkey), do: packet

  def encode(msg, sharedkey) do
    # IO.inspect("encode")
    # IO.inspect(Base.encode16(sharedkey), limit: :infinity)
    # IO.inspect(msg, limit: :infinity)
    # bin = :erlang.term_to_binary(msg)
    bin = :msgpack.pack(msg)
    iv = :crypto.strong_rand_bytes(@iv_bytes)

    {ciphertext, tag} =
      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        sharedkey,
        iv,
        bin,
        @seconds,
        @tag_bytes,
        true
      )

    iv <> tag <> ciphertext
  end

  @spec push(term()) :: :ok
  def push(msg) do
    id = :rand.uniform(1_000_000_000)
    msg = %{"id" => id, "msg" => msg}
    PubSub.local_broadcast(@pubsub_server, "echo", msg)
  end

  @spec push(term, term()) :: :ok
  def push(vid, msg) do
    id = :rand.uniform(1_000_000_000)
    msg = %{"id" => id, "msg" => msg}
    PubSub.local_broadcast(@pubsub_server, "echo:#{vid}", msg)
  end

  @spec push_except(term(), list()) :: :ok
  def push_except(msg, vids) do
    id = :rand.uniform(1_000_000_000)
    msg = %{"id" => id, "msg" => msg}
    PubSub.local_broadcast(@pubsub_server, "echo", {msg, vids})
  end

  def push_only(vid, msg) do
    PubSub.local_broadcast(@pubsub_server, "echo:#{vid}", msg)
  end

  @spec request(binary, term, integer, integer) :: {:ok, term} | {:error, term}
  def request(vid, msg, timeout \\ 10_000, retry \\ 0) do
    id = :rand.uniform(1_000_000_000)
    to = "echo:#{vid}"
    topic_callback = "res:#{id}"
    msg = %{"id" => id, "msg" => msg}

    PubSub.subscribe(@pubsub_server, topic_callback)
    PubSub.local_broadcast(@pubsub_server, to, msg)

    receive do
      result ->
        {:ok, result}
    after
      timeout ->
        if retry == 0 do
          PubSub.unsubscribe(@pubsub_server, topic_callback)
          {:error, :timeout}
        else
          request_retry(to, topic_callback, msg, timeout, retry - 1)
        end
    end
  end

  defp request_retry(to, topic, msg, timeout, retry) do
    PubSub.local_broadcast(@pubsub_server, to, msg)

    receive do
      result ->
        {:ok, result}
    after
      timeout ->
        if retry == 0 do
          PubSub.unsubscribe(@pubsub_server, topic)
          {:error, :timeout}
        else
          request_retry(to, topic, msg, timeout, retry - 1)
        end
    end
  end
end
