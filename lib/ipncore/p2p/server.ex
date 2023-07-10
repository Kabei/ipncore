defmodule Ippan.P2P.Server do
  use ThousandIsland.Handler
  alias Phoenix.PubSub
  require Logger

  @otp_app :ipncore

  @adapter ThousandIsland.Socket
  @timeout 45_000
  @handshake_timeout 5_000
  @seconds <<0>>
  @tag_bytes 16
  @pubsub_server :pubsub
  @version <<0, 0>>

  @spec load_kem :: :ok
  def load_kem do
    dir = Application.get_env(@otp_app, :kem_dir)

    seed =
      File.read!(dir)
      |> Fast64.decode64()

    {:ok, net_pubkey, net_privkey} = NtruKem.gen_key_pair_from_seed(seed)
    Application.put_env(@otp_app, :net_pubkey, net_pubkey)
    Application.put_env(@otp_app, :net_privkey, net_privkey)
  end

  def load_key do
    dir = Application.get_env(@otp_app, :key_dir)

    seed =
      File.read!(dir)
      |> Fast64.decode64()

    {:ok, {pubkey, privkey}} = Cafezinho.Impl.keypair_from_seed(seed)
    Application.put_env(@otp_app, :pubkey, pubkey)
    Application.put_env(@otp_app, :privkey, privkey)
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

  def handle_data(data, _socket, %{id: id} = state) do
    Logger.debug("data: #{data}")

    case decode(data, state) do
      {event, action, data} ->
        PubSub.broadcast(@pubsub_server, event, {action, Map.put(data, :vid, id)})

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
    msg = "WEL" <> @version <> Application.get_env(@otp_app, :net_pubkey)
    @adapter.send(socket, msg)

    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "THX" <> <<ciphertext::bytes-size(1278), encodeText::binary>>} ->
        IO.inspect("Thank")

        case NtruKem.dec(Application.get_env(@otp_app, :net_privkey), ciphertext) do
          {:ok, sharedkey} ->
            <<clientPubkey::bytes-size(32), id::64, signature::binary>> =
              decode(encodeText, sharedkey)

            case Cafezinho.Impl.verify(signature, sharedkey, clientPubkey) do
              :ok ->
                IO.inspect("id #{id}")

                case ValidatorStore.lookup([id]) do
                  nil ->
                    Logger.debug("[Server connection] validator not exists")
                    {:close, state}

                  %{hostname: hostname, name: name, pubkey: pubkey} ->
                    if pubkey != clientPubkey do
                      Logger.debug("[Server connection] Invalid handshake pubkey")
                      {:close, state}
                    else
                      Logger.debug("[Server connection] OK #{name} connected")

                      {:continue,
                       %{
                         id: id,
                         hostname: hostname,
                         net_pubkey: clientPubkey,
                         pubkey: pubkey,
                         sharedkey: sharedkey
                       }, @timeout}
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

      error ->
        Logger.debug("Invalid handshake")
        IO.inspect(error)
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
end
