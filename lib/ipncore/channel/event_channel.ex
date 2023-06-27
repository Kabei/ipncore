defmodule EventChannel do
  use GenServer
  alias Phoenix.PubSub
  require Logger

  @module __MODULE__
  @channel "event"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: @module)
  end

  def push(message) do
    GenServer.cast(@module, {:push, message})
  end

  @impl true
  def init(args) do
    IO.inspect(args)
    PubSub.subscribe(args.server, @channel)
    {:ok, args}
  end

  @impl true
  def handle_info({"valid", body}, state) do
    Logger.info("valid")
    IO.inspect(body, limit: :infinity)
    r = MessageStore.insert_sync(body)
    IO.inspect(r)
    hash = hd(body)
    PubSub.broadcast_from(state.server, self(), @channel, {"recv", hash})
    {:noreply, state}
  end

  def handle_info({"valid_df", body}, state) do
    Logger.info("valid_df")
    IO.inspect(body, limit: :infinity)
    r = MessageStore.insert_df(body)
    IO.inspect(r)
    hash = hd(body)
    PubSub.broadcast_from(state.server, self(), @channel, {"recv_df", hash})
    {:noreply, state}
  end

  def handle_info({"recv", hash}, state) do
    Logger.debug("recv #{Base.encode16(hash)}")
    MessageStore.delete_only(hash)
    {:noreply, state}
  end

  def handle_info({"recv_df", hash}, state) do
    Logger.debug("recv_df #{Base.encode16(hash)}")
    MessageStore.delete_df(hash)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:push, message}, state) do
    PubSub.broadcast_from(state.server, self(), @channel, message)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, state) do
    PubSub.unsubscribe(state.server, @channel)
  end
end
