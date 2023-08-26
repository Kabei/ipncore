defmodule Ippan.P2P.PeerClient do
  use GenServer, restart: :transient
  require Logger

  # @adapter :gen_tcp
  # @time_to_reconnect 3_000
  # @pubsub_server :network
  # @tcp_opts Application.compile_env(:ipncore, :p2p_client)
  # @module __MODULE__
  @ping_interval 45_000

  def start_link(_, args) do
    start_link(args)
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(args) do
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:ok, Map.put(args, :tRef, tRef), :hibernate}
  end

  @impl true
  def handle_info(:ping, %{socket: socket, adapter: adapter} = state) do
    adapter.send(socket, "PING")
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:noreply, Map.put(state, :tRef, tRef), :hibernate}
  end

  def handle_info({:tcp, socket, packet}, %{id: id, mod: mod, sharedkey: sharedkey} = state) do
    mod.on_message(id, socket, packet, sharedkey)

    {:noreply, state, :hibernate}
  end

  def handle_info({:tcp_closed, socket}, %{id: id, mod: mod, adapter: adapter} = state) do
    Logger.debug("tcp_closed | #{id}")
    adapter.close(socket)
    mod.on_disconnect(id)
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
