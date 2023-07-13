defmodule EventMinerChannel do
  use Channel,
    server: :miner,
    channel: "event"

  @send_to :verifiers

  @impl true
  def handle_info({"valid", from, body}, state) do
    hash = hd(body)
    Logger.debug("valid #{Base.encode16(hash)}")
    MessageStore.insert_sync(body)
    PubSub.broadcast(@send_to, "event:#{from}", {"recv", hash})
    {:noreply, state}
  end

  def handle_info({"valid_df", from, body}, state) do
    hash = Enum.at(body, 3)
    Logger.debug("valid_df #{Base.encode16(hash)}")
    MessageStore.insert_df(body)
    PubSub.broadcast(@send_to, "event:#{from}", {"recv_df", hash})
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
