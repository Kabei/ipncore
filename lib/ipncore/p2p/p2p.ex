defmodule Ippan.P2P do
  alias Phoenix.PubSub
  require Logger
  require Global

  @version <<0::16>>
  @seconds <<0>>
  @iv_bytes 12
  @tag_bytes 16
  @client_adapter :gen_tcp
  @server_adapter ThousandIsland.Socket
  @pubsub_server :network
  @handshake_timeout 5_000
  @server_ping_timeout 60_000

  def client_handshake(socket, state) do
    case @client_adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL" <> @version <> pubkey} ->
        {:ok, ciphertext, sharedkey} = NtruKem.enc(pubkey)

        id = Global.validator_id()
        {:ok, signature} = Cafezinho.Impl.sign(sharedkey, state.privkey)
        authtext = encode(state.pubkey <> <<id::64>> <> signature, sharedkey)
        @client_adapter.send(socket, "THX" <> ciphertext <> authtext)
        {:ok, sharedkey}

      {:error, :closed} ->
        :halt

      error ->
        error
    end
  end

  def server_handshake(socket, state, store_mod) do
    msg = "WEL" <> @version <> Global.net_pubkey()
    @server_adapter.send(socket, msg)

    case @server_adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "THX" <> <<ciphertext::bytes-size(1278), encodeText::binary>>} ->
        case NtruKem.dec(Global.net_privkey(), ciphertext) do
          {:ok, sharedkey} ->
            <<clientPubkey::bytes-size(32), id::64, signature::binary>> =
              decode!(encodeText, sharedkey)

            case Cafezinho.Impl.verify(signature, sharedkey, clientPubkey) do
              :ok ->
                case apply(store_mod, :lookup_map, [id]) do
                  nil ->
                    Logger.error("validator not exists #{id}")
                    {:close, state}

                  %{hostname: hostname, name: name, pubkey: pubkey} ->
                    if pubkey != clientPubkey do
                      Logger.debug("[Server connection] Invalid handshake pubkey")
                      {:close, state}
                    else
                      Logger.debug("[Server connection] #{name} connected")

                      {:continue,
                       %{
                         id: id,
                         hostname: hostname,
                         net_pubkey: clientPubkey,
                         pubkey: pubkey,
                         sharedkey: sharedkey
                       }, @server_ping_timeout}
                    end
                end

              _ ->
                Logger.debug("Invalid signature authentication")
                {:close, state}
            end

          _error ->
            Logger.debug("Invalid ntrukem ciphertext authentication")
            {:close, state}
        end

      _error ->
        Logger.debug("Invalid handshake")
        # IO.inspect(error)
        {:close, state}
    end
  end

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
    |> CBOR.decode()
    |> elem(1)
  end

  def decode!(packet, _sharedkey), do: packet

  def encode(msg, sharedkey) do
    # IO.inspect("encode")
    # IO.inspect(Base.encode16(sharedkey), limit: :infinity)
    # IO.inspect(msg, limit: :infinity)
    # bin = :erlang.term_to_binary(msg)
    bin = CBOR.encode(msg)
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
  def request(vid, method, timeout \\ 10_000, retry \\ 0) do
    id = :rand.uniform(1_000_000_000)
    to = "echo:#{vid}"
    topic_callback = "res:#{id}"
    msg = %{"id" => id, "method" => method}

    PubSub.subscribe(@pubsub_server, topic_callback)
    PubSub.local_broadcast(@pubsub_server, to, msg)

    receive do
      result ->
        PubSub.unsubscribe(@pubsub_server, topic_callback)
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
