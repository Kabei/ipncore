defmodule NodeMonitor do
  alias Phoenix.PubSub
  use GenServer
  require Logger

  @module __MODULE__
  @backoff 1_500

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: __MODULE__)
  end

  @impl true
  def init(peer) do
    :ok = :net_kernel.monitor_nodes(true)

    {:ok, %{peer: peer, mailbox: %{}, conn: false}, {:continue, :init}}
  end

  @impl true
  def handle_continue(:init, %{peer: peer} = state) do
    send(self(), {:connect, peer})

    {:noreply, state}
  end

  @impl true
  def handle_info({:nodedown, node_name}, %{peer: peer} = state) do
    Logger.debug(inspect({:nodedown, node_name}))

    if node_name == peer do
      :timer.send_after(@backoff, {:connect, node_name})

      {:noreply, %{state | conn: false}}
    else
      {:noreply, state}
    end
  end

  def handle_info({:nodeup, node_name}, state) do
    Logger.debug(inspect({:nodeup, node_name}))
    {:noreply, %{state | conn: true}}
  end

  def handle_info({:connect, node_name}, %{mailbox: mailbox, peer: peer} = state) do
    if node_name == peer do
      case Node.connect(node_name) do
        false ->
          :timer.send_after(@backoff, {:connect, node_name})

        true ->
          on_connect_spawn(self(), node_name, mailbox)
      end
    end

    {:noreply, state}
  end

  def handle_info(:clear, state) do
    {:noreply, %{state | mailbox: %{}}}
  end

  @impl true
  def handle_cast(
        {:push, pubsub, topic, message},
        %{conn: conn, mailbox: mailbox, peer: peer} = state
      ) do
    if conn do
      PubSub.direct_broadcast(peer, pubsub, topic, message)
      {:noreply, state}
    else
      {:noreply,
       %{
         state
         | mailbox: Map.put(mailbox, :rand.uniform(1_000_000_000), {pubsub, topic, message})
       }}
    end
  end

  @impl true
  def terminate(_reason, _state) do
    :net_kernel.monitor_nodes(false)
  end

  def push(pubsub, topic, message) do
    GenServer.cast(@module, {:push, pubsub, topic, message})
  end

  defp on_connect_spawn(_pid, _node_name, %{}), do: :ok
  defp on_connect_spawn(pid, peer, mailbox) do
    spawn_link(fn ->
      for {_key, {pubsub, topic, message}} <- mailbox do
        PubSub.direct_broadcast(peer, pubsub, topic, message)
      end

      GenServer.cast(pid, :clear)
    end)
  end
end
