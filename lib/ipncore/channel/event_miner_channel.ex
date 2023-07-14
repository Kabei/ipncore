defmodule EventMinerChannel do
  use Channel,
    server: :miner,
    channel: "event"

  @send_to :verifiers

  @impl true
  def handle_info({"valid", from, hash, body}, state) do
    Logger.debug("valid #{Base.encode16(hash)}")
    status = MessageStore.insert_sync(body)
    PubSub.broadcast(@send_to, "event:#{from}", {"recv", hash, status})
    {:noreply, state}
  end

  def handle_info({"valid_df", from, hash, body}, state) do
    Logger.debug("valid_df #{Base.encode16(hash)}")
    status = MessageStore.insert_df(body)
    PubSub.broadcast(@send_to, "event:#{from}", {"recv_df", hash, status})
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
