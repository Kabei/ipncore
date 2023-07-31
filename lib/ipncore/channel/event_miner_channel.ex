defmodule EventMinerChannel do
  use Channel,
    server: :verifiers,
    topic: "event"

  @impl true
  def handle_info({"valid", from, [hash, timestamp | _] = body}, state) do
    Logger.debug("valid #{Base.encode16(hash)}")

    case MessageStore.insert_hash(hash, timestamp) do
      1 ->
        MessageStore.insert_sync(body)
        PubSub.direct_broadcast(from, :verifiers, "event", {"recv", hash, :ok})

      _ ->
        PubSub.direct_broadcast(from, :verifiers, "event", {"recv", hash, :error})
    end

    {:noreply, state}
  end

  def handle_info({"valid_df", from, [hash, timestamp | _] = body}, state) do
    Logger.debug("valid_df #{Base.encode16(hash)}")

    case MessageStore.insert_hash(hash, timestamp) do
      1 ->
        MessageStore.insert_df(body)
        PubSub.direct_broadcast(from, :verifiers, "event", {"recv", hash, :ok})

      _ ->
        PubSub.direct_broadcast(from, :verifiers, "event", {"recv", hash, :error})
    end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
