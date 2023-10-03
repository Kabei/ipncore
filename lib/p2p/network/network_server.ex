defmodule Ippan.NetworkServer do
  use ThousandIsland.Handler
  alias Ippan.P2P
  alias Ippan.NetworkNodes
  require Logger

  @impl ThousandIsland.Handler
  def handle_connection(socket, state) do
    # Logger.debug("handle_connection #{inspect(state)}")
    tcp_socket = socket.socket

    case P2P.server_handshake(
           tcp_socket,
           :persistent_term.get(:net_privkey),
           &NetworkNodes.fetch/1
         ) do
      {:ok, id, %{sharedkey: sharedkey} = node, timeout} ->
        NetworkNodes.on_connect(id, node)

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
    NetworkNodes.on_message(packet, state)

    {:continue, state}
  end

  @impl ThousandIsland.Handler
  def handle_close(_socket, state) do
    Logger.debug("handle close socket")

    NetworkNodes.on_disconnect(state)
    {:close, state}
  end

  @impl ThousandIsland.Handler
  def handle_shutdown(_socket, state) do
    Logger.debug("handle shutdown")
    NetworkNodes.on_disconnect(state)
    {:close, state}
  end

  @impl ThousandIsland.Handler
  def handle_timeout(_socket, state) do
    Logger.debug("handle timeout")
    NetworkNodes.on_disconnect(state)
    {:close, state}
  end
end
