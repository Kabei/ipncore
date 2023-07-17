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

  def handle_data(
        data,
        socket,
        %{id: vid, pubkey: pubkey, sharedkey: sharedkey} = state
      ) do
    try do
      from = %{id: vid, pubkey: pubkey}

      case decode!(data, sharedkey) do
        %{id: id, msg: msg} ->
          case msg do
            {"new_recv", rest} ->
              send(VoteCounter, {"new_recv", from, rest})
              @adapter.send(socket, encode(%{id: id, status: :ok}, sharedkey))

            {"vote", rest} ->
              send(VoteCounter, {"vote", from, rest})
              @adapter.send(socket, encode(%{id: id, status: :ok}, sharedkey))

            _ ->
              :ok
          end

        _ ->
          :ok
      end

      # PubSub.local_broadcast(:verifiers, "block", {action, from, rest})
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
end
