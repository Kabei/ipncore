defmodule EventVerifierChannel do
  use Channel,
    server: :verifiers,
    channel: "event"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    PubSub.subscribe(@pubsub_server, "#{@channel}:#{node()}")
    {:ok, args}
  end

  @impl true
  def handle_info({"recv", hash, status}, state) do
    Logger.debug("recv #{Base.encode16(hash)}")
    MessageStore.delete_only(hash)
    PubSub.broadcast(:verifiers, "event:#{hash}", status)
    {:noreply, state}
  end

  def handle_info({"recv_df", hash, status}, state) do
    Logger.debug("recv_df #{Base.encode16(hash)}")
    MessageStore.delete_df(hash)
    PubSub.broadcast(:verifiers, "event:#{hash}", status)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
