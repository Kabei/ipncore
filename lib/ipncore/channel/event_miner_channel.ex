defmodule EventMinerChannel do
  use Channel,
    server: :cluster,
    topic: "event"

  @impl true
  def handle_info({"valid", from, [hash | _] = body}, state) do
    Logger.debug("valid #{Base.encode16(hash)}")

    case MessageStore.insert(body) do
      :done ->
        PubSub.direct_broadcast(from, :cluster, "event", {"recv", hash, :ok})

      _ ->
        PubSub.direct_broadcast(from, :cluster, "event", {"recv", hash, :error})
    end

    {:noreply, state}
  end

  def handle_info(
        {"valid_df", from, [hash | _] = body},
        state
      ) do
    Logger.debug("valid_df #{Base.encode16(hash)}")

    case MessageStore.insert_df(body) do
      :done ->
        PubSub.direct_broadcast(from, :cluster, "event", {"recv", hash, :ok})

      _ ->
        PubSub.direct_broadcast(from, :cluster, "event", {"recv", hash, :error})
    end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end
end
