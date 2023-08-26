defmodule Ippan.P2P do
  require Logger
  require BigNumber

  @version <<0::16>>
  @seconds <<0>>
  @iv_bytes 12
  @tag_bytes 16
  @adapter :gen_tcp
  @handshake_timeout 5_000
  @server_ping_timeout 60_000

  @spec client_handshake(
          socket :: term,
          node_id :: integer(),
          kem_pubkey :: binary(),
          privkey :: binary()
        ) ::
          {:ok, sharekey :: binary} | {:error, term()} | :halt
  def client_handshake(socket, node_id, kem_pubkey, privkey) do
    {:ok, ciphertext, sharedkey} = NtruKem.enc(kem_pubkey)
    {:ok, signature} = Cafezinho.Impl.sign(sharedkey, privkey)
    authtext = encode(signature <> <<node_id::unsigned-size(64)>>, sharedkey)
    @adapter.send(socket, "HI" <> @version <> ciphertext <> authtext)

    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL"} ->
        {:ok, sharedkey}

      {:ok, _wrong} ->
        @adapter.close(socket)
        :halt

      error ->
        error
    end
  end

  @spec server_handshake(socket :: term, kem_privkey :: binary, fun :: fun()) ::
          tuple() | :error
  def server_handshake(socket, kem_privkey, fun) do
    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "HI" <> @version <> <<ciphertext::bytes-size(1278), encodeText::binary>>} ->
        case NtruKem.dec(kem_privkey, ciphertext) do
          {:ok, sharedkey} ->
            <<signature::bytes-size(64), id::unsigned-size(64)>> = decode!(encodeText, sharedkey)

            case fun.(id) do
              %{name: name, hostname: hostname, pubkey: clientPubkey, net_pubkey: net_pubkey} ->
                case Cafezinho.Impl.verify(signature, sharedkey, clientPubkey) do
                  :ok ->
                    Logger.debug("[Server connection] #{name} connected")
                    @adapter.send(socket, "WEL")

                    {:ok, id, hostname, sharedkey, net_pubkey, @server_ping_timeout}

                  _ ->
                    Logger.debug("Invalid signature authentication")
                    :error
                end

              _ ->
                Logger.debug("validator not exists #{id}")
                :error
            end

          _error ->
            Logger.debug("Invalid ntrukem ciphertext authentication")
            :error
        end

      _error ->
        Logger.debug("Invalid handshake")
        # IO.inspect(error)
        :error
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
    |> CBOR.Decoder.decode()
    |> elem(0)
  end

  def decode!(packet, _sharedkey), do: packet

  def encode(msg, sharedkey) do
    bin = CBOR.Encoder.encode_into(msg, <<>>)
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

  # @spec push(term()) :: :ok
  # def push(msg) do
  #   id = :rand.uniform(1_000_000_000)
  #   msg = %{"id" => id, "msg" => msg}
  #   PubSub.local_broadcast(@pubsub_server, "echo", msg)
  # end

  # @spec push(term, term()) :: :ok
  # def push(vid, msg) do
  #   id = :rand.uniform(1_000_000_000)
  #   msg = %{"id" => id, "msg" => msg}
  #   PubSub.local_broadcast(@pubsub_server, "echo:#{vid}", msg)
  # end

  # @spec push_except(term(), list()) :: :ok
  # def push_except(msg, vids) do
  #   id = :rand.uniform(1_000_000_000)
  #   msg = %{"id" => id, "msg" => msg}
  #   PubSub.local_broadcast(@pubsub_server, "echo", {msg, vids})
  # end

  # def push_only(vid, msg) do
  #   PubSub.local_broadcast(@pubsub_server, "echo:#{vid}", msg)
  # end

  # @spec request(binary, term, integer, integer) :: {:ok, term} | {:error, term}
  # def request(vid, method, timeout \\ 10_000, retry \\ 0) do
  #   id = :rand.uniform(1_000_000_000)
  #   to = "echo:#{vid}"
  #   topic_callback = "res:#{id}"
  #   msg = %{"id" => id, "method" => method}

  #   PubSub.subscribe(@pubsub_server, topic_callback)
  #   PubSub.local_broadcast(@pubsub_server, to, msg)

  #   receive do
  #     result ->
  #       PubSub.unsubscribe(@pubsub_server, topic_callback)
  #       {:ok, result}
  #   after
  #     timeout ->
  #       if retry == 0 do
  #         PubSub.unsubscribe(@pubsub_server, topic_callback)
  #         {:error, :timeout}
  #       else
  #         request_retry(to, topic_callback, msg, timeout, retry - 1)
  #       end
  #   end
  # end

  # defp request_retry(to, topic, msg, timeout, retry) do
  #   PubSub.local_broadcast(@pubsub_server, to, msg)

  #   receive do
  #     result ->
  #       {:ok, result}
  #   after
  #     timeout ->
  #       if retry == 0 do
  #         PubSub.unsubscribe(@pubsub_server, topic)
  #         {:error, :timeout}
  #       else
  #         request_retry(to, topic, msg, timeout, retry - 1)
  #       end
  #   end
  # end
end
