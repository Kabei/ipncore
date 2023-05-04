defmodule Ippan.P2P.EventHandler do
  alias Phoenix.PubSub
  require Logger
  use GenServer, restart: :temporary

  @module __MODULE__
  @pubsub_server :pubsub

  def start_link(opts) do
    GenServer.start_link(@module, opts)
  end

  @impl true
  def init(opts) do
    Process.flag(:trap_exit, true)
    subscribe()
    {:ok, opts}
  end

  @impl true
  def handle_info(%{event: "block.new", from: from} = msg, state) do
    {:noreply, state}
  end

  def handle_info(%{event: "block.received", from: from} = msg, state) do
    {:noreply, state}
  end

  def handle_info(%{event: "round.start", from: from} = msg, state) do
    {:noreply, state}
  end

  def handle_info(%{event: "round.end", from: from} = msg, state) do
    {:noreply, state}
  end

  def handle_info(%{event: "node.status", from: from} = msg, state) do
    {:noreply, state}
  end

  def handle_info({:EXIT, _pid, {:exit_trap, reason}}, state) do
    {:stop, reason, state}
  end

  def handle_info(_, state) do
    Logger.debug("No exist eventHandler")
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    unsubscribe()
    :ok
  end

  defp subscribe do
    PubSub.subscribe(@pubsub_server, "block.new")
    PubSub.subscribe(@pubsub_server, "block.received")
    PubSub.subscribe(@pubsub_server, "round.start")
    PubSub.subscribe(@pubsub_server, "round.end")
    PubSub.subscribe(@pubsub_server, "node.status")
  end

  defp unsubscribe do
    PubSub.unsubscribe(@pubsub_server, "block.new")
    PubSub.unsubscribe(@pubsub_server, "block.received")
    PubSub.unsubscribe(@pubsub_server, "round.start")
    PubSub.unsubscribe(@pubsub_server, "round.end")
    PubSub.unsubscribe(@pubsub_server, "node.status")
  end
end
