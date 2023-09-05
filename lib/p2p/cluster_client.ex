defmodule Ippan.ClusterClient do
  use GenServer, restart: :transient
  require Logger

  @adapter :gen_tcp
  @node Ippan.ClusterNode
  @ping_interval 45_000

  def start_link(_, args) do
    start_link(args)
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, [hibernate_after: 2_000])
  end

  @impl true
  def init(args) do
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:ok, Map.put(args, :tRef, tRef), :hibernate}
  end

  @impl true
  def handle_info(:ping, %{socket: socket} = state) do
    @adapter.send(socket, "PING")
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:noreply, Map.put(state, :tRef, tRef), :hibernate}
  end

  def handle_info({:tcp, _socket, packet}, state) do
    @node.on_message(packet, state)

    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, %{id: id} = state) do
    Logger.debug("tcp_closed | #{id}")
    @adapter.close(socket)
    @node.on_disconnect(state)
    {:stop, state}
  end

  if Mix.env() == :dev do
    def handle_info({:tcp_error, _socket, reason}, %{id: id} = state) do
      Logger.debug("tcp_error #{reason} | #{id}")
      {:noreply, state}
    end
  end

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
  end
end
