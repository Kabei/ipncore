defmodule NodeMonitor do
  alias Phoenix.PubSub
  use GenServer
  require Logger

  @pubsub_server :verifiers
  @topic "event"
  @backoff 1_500

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: __MODULE__)
  end

  @impl true
  def init(node_list) do
    :ok = :net_kernel.monitor_nodes(true)
    {:ok, node_list, {:continue, :init}}
  end

  @impl true
  def handle_continue(:init, node_list) do
    for n <- node_list do
      send(self(), {:connect, n})
    end

    {:noreply, node_list}
  end

  @impl true
  def handle_info({:nodedown, node_name}, state) do
    Logger.debug(inspect({:nodedown, node_name}))

    :timer.send_after(@backoff, {:connect, node_name})
    {:noreply, state}
  end

  def handle_info({:nodeup, node_name}, state) do
    Logger.debug(inspect({:nodeup, node_name}))
    {:noreply, state}
  end

  def handle_info({:connect, node_name}, state) do
    case Node.connect(node_name) do
      false ->
        :timer.send_after(@backoff, {:connect, node_name})

      true ->
        on_connect_spawn(node_name)
    end

    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :net_kernel.monitor_nodes(false)
  end

  defp on_connect_spawn(node_name) do
    spawn_link(fn ->
      {:ok, data} = MessageStore.all()
      from = node()

      for msg <- data do
        PubSub.direct_broadcast(node_name, @pubsub_server, @topic, {"valid", from, msg})
      end

      {:ok, data} = MessageStore.all_df()

      for msg <- data do
        PubSub.direct_broadcast(node_name, @pubsub_server, @topic, {"valid_df", from, msg})
      end
    end)
  end
end
