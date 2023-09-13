defmodule Ippan.NetworkServer do
  use ThousandIsland.Handler
  alias Ippan.P2P
  alias Ippan.NetworkNode
  require Logger

  @impl ThousandIsland.Handler
  def handle_connection(socket, state) do
    # Logger.debug("handle_connection #{inspect(state)}")
    tcp_socket = socket.socket

    case P2P.server_handshake(
           tcp_socket,
           :persistent_term.get(:net_privkey),
           &NetworkNode.fetch/1
         ) do
      {:ok, id, hostname, sharedkey, net_pubkey, timeout} ->
        NetworkNode.on_connect(id, %{
          socket: tcp_socket,
          sharedkey: sharedkey,
          hostname: hostname,
          net_pubkey: net_pubkey
        })

        {:continue, %{id: id, socket: tcp_socket, sharedkey: sharedkey}, timeout}

      _ ->
        {:close, state}
    end
  end

  @impl ThousandIsland.Handler
  def handle_data("PING", _socket, state) do
    # Logger.debug("PING")
    {:continue, state}
  end

  # event data | id method data
  def handle_data(packet, _socket, state) do
    NetworkNode.on_message(packet, state)

    {:continue, state}
  end

  @impl ThousandIsland.Handler
  def handle_close(_socket, state) do
    Logger.debug("handle close socket")

    NetworkNode.on_disconnect(state)
    {:close, state}
  end

  @impl ThousandIsland.Handler
  def handle_shutdown(_socket, state) do
    Logger.debug("handle shutdown")
    NetworkNode.on_disconnect(state)
    {:close, state}
  end

  @impl ThousandIsland.Handler
  def handle_timeout(_socket, state) do
    Logger.debug("handle timeout")
    NetworkNode.on_disconnect(state)
    {:close, state}
  end
end
