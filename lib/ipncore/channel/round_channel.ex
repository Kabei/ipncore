defmodule RoundChannel do
  use GenServer
  alias Phoenix.PubSub

  @pubsub_server :verifiers
  @channel "round"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: __MODULE__)
  end

  @impl true
  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    {:ok, args}
  end

  @impl true
  def handle_info({"start", _id}, state) do
    BlockTimer.start()
    {:noreply, state}
  end

  def handle_info({"end", opts}, state) do
    send(self(), {"start", opts.id + 1})
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_server, @channel)
  end
end
