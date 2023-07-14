defmodule Ippan.P2P.Server do
  use ThousandIsland.Handler
  alias Phoenix.PubSub
  require Logger

  @otp_app :ipncore
  @adapter ThousandIsland.Socket
  @version <<0, 0>>
  @seconds <<0>>
  @iv_bytes 12
  @tag_bytes 16
  @handshake_timeout 5_000
  @timeout 45_000

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

  @impl ThousandIsland.Handler
  def handle_connection(socket, state) do
    Logger.debug("handle_connection #{inspect(state)}")
    handshake(socket, state)
  end

  @impl ThousandIsland.Handler
  def handle_data("PING", _socket, state) do
    {:continue, state}
  end

  def handle_data(packet, _socket, %{id: id, sharedkey: sharedkey} = state) do
    Logger.debug("data: #{inspect(packet, limit: :infinity)}")

    try do
      for data <- normalize_packet(packet, []) do
        case decode!(data, sharedkey) do
          {"block", "recv", block_id} ->
            PubSub.broadcast(:network, "msg:#{id}", {"clear", block_id})

          {"block", action, data} ->
            PubSub.broadcast(:miner, "block", {action, id, data})
            PubSub.broadcast(:network, "msg", {"block", "recv", Map.get(data, :height)})

          _ ->
            :ok
        end
      end
    rescue
      e ->
        # Logger.error(inspect(e))
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
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
        # IO.inspect("Thank")

        case NtruKem.dec(Application.get_env(@otp_app, :net_privkey), ciphertext) do
          {:ok, sharedkey} ->
            <<clientPubkey::bytes-size(32), id::64, signature::binary>> =
              decode!(encodeText, sharedkey)

            case Cafezinho.Impl.verify(signature, sharedkey, clientPubkey) do
              :ok ->
                case ValidatorStore.lookup([id]) do
                  nil ->
                    Logger.debug("[Server connection] validator not exists")
                    {:close, state}

                  %{hostname: hostname, name: name, pubkey: pubkey} ->
                    if pubkey != clientPubkey do
                      Logger.debug("[Server connection] Invalid handshake pubkey")
                      {:close, state}
                    else
                      Logger.debug(
                        "[Server connection] OK #{name} connected #{Base.encode16(sharedkey)}"
                      )

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

      _error ->
        Logger.debug("Invalid handshake")
        # IO.inspect(error)
        {:close, state}
    end
  end

  defp decode!(packet, sharedkey) do
    <<iv::bytes-size(@iv_bytes), tag::bytes-size(@tag_bytes), ciphertext::binary>> = packet
    IO.inspect("decode")
    IO.inspect(Base.encode16(sharedkey), limit: :infinity)
    IO.inspect(packet, limit: :infinity)

    r =
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

    IO.inspect(r, limit: :infinity)

    r
  end

  defp normalize_packet(<<size::16, rest::binary>>, acc) do
    try do
      <<msg::bytes-size(size), new_rest::binary>> = rest
      normalize_packet(new_rest, acc ++ [msg])
    rescue
      _ ->
        acc ++ [rest]
    end
  end

  defp normalize_packet(_, acc), do: acc
end
