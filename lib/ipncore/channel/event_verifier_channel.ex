defmodule EventVerifierChannel do
  use Channel,
    server: :verifiers,
    topic: "event"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @topic)
    PubSub.subscribe(@pubsub_server, "#{@topic}:#{node()}")
    {:ok, args}
  end

  @impl true
  def handle_info({"recv", hash, status}, state) do
    hash16 = Base.encode16(hash)
    Logger.debug("recv #{hash16}")
    MessageStore.delete_only(hash)
    PubSub.broadcast(:verifiers, "event:#{hash16}", status)
    {:noreply, state}
  end

  def handle_info({"recv_df", hash, status}, state) do
    hash16 = Base.encode16(hash)
    Logger.debug("recv_df #{hash16}")
    MessageStore.delete_df(hash)
    PubSub.broadcast(:verifiers, "event:#{hash16}", status)
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
