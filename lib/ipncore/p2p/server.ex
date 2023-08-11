defmodule Ippan.P2P.Server do
  use ThousandIsland.Handler
  import Ippan.P2P, only: [decode!: 2, encode: 2]
  alias Ippan.P2P
  # alias Phoenix.PubSub
  require Logger

  @otp_app :ipncore
  @adapter ThousandIsland.Socket
  @version P2P.version()
  @handshake_timeout P2P.handshake_timeout()
  @timeout 60_000
  # @pubsub_server :network

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
    # Logger.debug("handle_connection #{inspect(state)}")
    handshake(socket, state)
  end

  @impl ThousandIsland.Handler
  def handle_data("PING", _socket, state) do
    {:continue, state}
  end

  # event data | id method data
  def handle_data(
        packet,
        socket,
        %{id: vid, pubkey: pubkey, sharedkey: sharedkey} = state
      ) do
    try do
      case decode!(packet, sharedkey) do
        %{"event" => event, "data" => data} ->
          from = %{id: vid, pubkey: pubkey}
          handle_event(event, from, data)

        %{"id" => id, "method" => method, "data" => data} ->
          case handle_request(method, data) do
            :not_found ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "error", "data" => 404}, sharedkey)
              )

            response ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "ok", "data" => response}, sharedkey)
              )
          end

        %{"id" => id, "method" => method} ->
          case handle_request(method) do
            :not_found ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "error", "data" => 404}, sharedkey)
              )

            response ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "ok", "data" => response}, sharedkey)
              )
          end

        result ->
          Logger.warning(inspect(result))
          :ok
      end
    rescue
      e ->
        # Logger.error(inspect(e))
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
    catch
      :exit, msg ->
        Logger.debug(inspect(msg))
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
        case NtruKem.dec(Application.get_env(@otp_app, :net_privkey), ciphertext) do
          {:ok, sharedkey} ->
            <<clientPubkey::bytes-size(32), id::64, signature::binary>> =
              decode!(encodeText, sharedkey)

            case Cafezinho.Impl.verify(signature, sharedkey, clientPubkey) do
              :ok ->
                case ValidatorStore.lookup_map(id) do
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

  defp handle_event("block_msg", from, data) do
    block = MapUtil.to_existing_atoms(data)
    send(VoteCounter, {"new_recv", from, true, block})
  end

  defp handle_event(_, _, _), do: :ok

  defp handle_request("get_state") do
    BlockTimer.get_state()
  end

  defp handle_request(_), do: :not_found

  defp handle_request("get_block_messages", %{"round" => _round}) do
    :ok
  end

  defp handle_request(_, _) do
    :not_found
  end
end
