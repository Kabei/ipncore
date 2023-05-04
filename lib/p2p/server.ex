defmodule ServerData do
  use GlobalConst.DummyModule
end

defmodule Ippan.P2P.Server do
  alias Ipnutils.Address
  alias Phoenix.PubSub
  alias Ipncore.Address
  use ThousandIsland.Handler
  require Logger

  @adapter ThousandIsland.Socket
  @timeout 30_000
  @handshake_timeout 5_000
  @seconds <<0>>
  @tag_bytes 16
  @pubsub_server :pubsub
  @version <<0, 0>>

  def load do
    value = Application.get_env(:ipncore, :kem_dir, "priv/cert/kem.key")
    dir = System.get_env("KEM_DIR", value)

    seed =
      File.read!(dir)
      |> Base.decode64!()

    {:ok, pubkey, privkey} = NtruKem.gen_key_pair_from_seed(seed)

    GlobalConst.new(ServerData, %{pubkey: pubkey, privkey: privkey})
  end

  def send(pid, event, msg) do
    message = Map.put(msg, :event, event)
    GenServer.cast(pid, {:send, message})
  end

  def handle_cast({:send, data}, %{socket: socket} = state) do
    message = encode(data, state)
    @adapter.send(socket, message)

    {:noreply, state}
  end

  @impl ThousandIsland.Handler
  def handle_connection(socket, state) do
    handshake(socket, state)
  end

  @impl ThousandIsland.Handler
  def handle_data("PING", _socket, state) do
    # Logger.debug("ping")
    {:continue, state}
  end

  def handle_data(data, socket, state) do
    Logger.debug("data: #{data}")

    message = decode(data, state)
    Logger.debug("event: #{inspect(message)}")

    case message do
      %{event: "block.sendfile", id: block_id} ->
        send_blockfile(socket, block_id)

      %{event: event} ->
        rest = Map.delete(message, :event)
        PubSub.broadcast(@pubsub_server, event, rest)

      _ ->
        :ok
    end

    {:continue, state}
  end

  @impl ThousandIsland.Handler
  def handle_close(_socket, _state) do
    Logger.debug("handle close socket")

    :ok
  end

  @impl ThousandIsland.Handler
  def handle_shutdown(_socket, _state) do
    Logger.debug("handle shutdown")

    :ok
  end

  @impl ThousandIsland.Handler
  def handle_timeout(_socket, _state) do
    Logger.debug("handle timeout")
    :ok
  end

  defp handshake(socket, state) do
    msg = "WEL" <> @version <> state.pubkey
    @adapter.send(socket, msg)

    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "THX" <> <<ciphertext::bytes-size(1278), encodeText::binary>>} ->
        case NtruKem.dec(state.privkey, ciphertext) do
          {:ok, sharedkey} ->
            <<clientPubkey::1138, signature::binary>> = decode(encodeText, sharedkey)
            address = Address.hash(clientPubkey)

            case Falcon.verify(msg, signature, clientPubkey) do
              :ok ->
                case Validators.by_address(address) do
                  nil ->
                    {:close, state}

                  validator ->
                    {:continue,
                     %{
                       id: validator.id,
                       address: address,
                       pubkey: clientPubkey,
                       sharedkey: sharedkey
                     }, @timeout}
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
        {:close, state}
    end
  end

  defp encode(msg, sharedkey) do
    bin = :erlang.term_to_binary(msg)
    iv = :crypto.strong_rand_bytes(12)

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

  defp decode(packet, sharedkey) do
    <<iv::bytes-size(12), tag::bytes-size(16), ciphertext::binary>> = packet

    :crypto.crypto_one_time_aead(
      :chacha20_poly1305,
      sharedkey,
      iv,
      ciphertext,
      @seconds,
      tag,
      false
    )
    |> :erlang.binary_to_term([:safe])
  end

  def send_blockfile(%{socket: socket} = _client, block_id) do
    filename = Path.join("/data/blocks", to_string(block_id))

    if File.regular?(filename) do
      :gen_tcp.send(socket, "blockfile:ok:#{block_id}")
      :file.sendfile(filename, socket)
    else
      :gen_tcp.send(socket, "blockfile:error")
    end
  end
end
