defmodule BlockChannel do
  use GenServer
  alias Phoenix.PubSub

  @module __MODULE__
  @pubsub_server :network
  @channel "block"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: @module)
  end

  def push(message) do
    GenServer.cast(@module, {:push, message})
  end

  @impl true
  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    {:ok, args}
  end

  @impl true
  def handle_info({"new", _block}, state) do
    {:noreply, state}
  end

  def handle_info({"recv", _block}, state) do
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:push, message}, state) do
    PubSub.broadcast_from(@pubsub_server, self(), @channel, message)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_server, @channel)
  end
end
