defmodule Ippan.P2P.ClientPool do
  require Logger
  use GenServer
  alias Ippan.P2P.Client
  alias Phoenix.PubSub

  @port Application.compile_env(:ipncore, :port, 5815)
  @pubsub_server :pubsub
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, hibernate_after: 5_000)
  end

  @impl true
  def init(key_path) do
    Process.flag(:trap_exit, true)
    {:ok, validators} = ValidatorStore.all()
    myid = Default.validator_id()

    clients =
      for validator when validator != myid <- validators, into: %{} do
        hostname = validator.hostname
        {:ok, pid} = Client.start_link({hostname, @port, key_path})
        {validator.id, %{pid: pid, hostname: hostname}}
      end

    subscribe()

    {:ok, %{clients: clients, key_path: key_path}}
  end

  @impl true
  def handle_info({:EXIT, _pid, {:exit_trap, reason}}, %{clients: clients} = state) do
    unsubscribe()
    unlink(clients)
    {:stop, reason, state}
  end

  def handle_info(%{event: "validator.new", data: validator}, %{
        clients: clients,
        key_path: key_path
      }) do
    hostname = validator.hostname
    {:ok, pid} = Client.start_link({hostname, @port, key_path})

    new_state = Map.put(clients, validator.id, %{pid: pid, hostname: hostname})
    {:noreply, new_state}
  end

  def handle_info(
        %{event: "validator.update", data: {id, map}},
        %{clients: clients, key_path: key_path} = state
      ) do
    case Map.get(map, :hostname) do
      nil ->
        {:noreply, state}

      hostname ->
        %{pid: old_pid} = Map.get(clients, id)
        GenServer.stop(old_pid, :normal)
        {:ok, pid} = Client.start_link({hostname, @port, key_path})
        new_state = Map.put(clients, id, %{pid: pid, hostname: hostname})
        {:noreply, new_state}
    end
  end

  def handle_info(%{event: "validator.delete", data: id}, %{clients: clients}) do
    %{pid: pid} = Map.get(clients, id)
    GenServer.stop(pid, :normal)
    {:noreply, Map.delete(clients, id)}
  end

  @impl true
  def terminate(_reason, state) do
    unsubscribe()
    unlink(state)
    :ok
  end

  defp unlink(%{clients: clients}) do
    for {_id, %{pid: pid}} <- clients do
      GenServer.stop(pid, :normal)
    end
  end

  defp unlink(_), do: :ok

  defp subscribe do
    PubSub.subscribe(@pubsub_server, "validator.new")
    PubSub.subscribe(@pubsub_server, "validator.update")
    PubSub.subscribe(@pubsub_server, "validator.delete")
  end

  defp unsubscribe do
    PubSub.unsubscribe(@pubsub_server, "validator.new")
    PubSub.unsubscribe(@pubsub_server, "validator.update")
    PubSub.unsubscribe(@pubsub_server, "validator.delete")
  end
end
