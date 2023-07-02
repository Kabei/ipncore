defmodule Ippan.P2P.ClientPool do
  require Logger
  use GenServer
  alias Ippan.Validator
  alias Ippan.P2P.Client
  alias Phoenix.PubSub

  @port Application.compile_env(:ipncore, :port, 5815)
  @pubsub_server :network
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, hibernate_after: 5_000)
  end

  @impl true
  def init(key_path) do
    # Process.flag(:trap_exit, true)
    {:ok, validators} = ValidatorStore.all()
    myid = Default.validator_id()

    clients =
      for validator when validator != myid <- validators, into: %{} do
        validator = Validator.to_map(validator)
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

  def handle_info(
        {"new", validator},
        %{
          clients: clients,
          key_path: key_path
        } = state
      ) do
    if Default.validator_id() != validator.id do
      hostname = validator.hostname
      {:ok, pid} = Client.start_link({hostname, @port, key_path})

      new_state = Map.put(clients, validator.id, %{pid: pid, hostname: hostname})
      {:noreply, new_state}
    else
      {:noreply, state}
    end
  end

  def handle_info(
        {"update", %{id: validator_id, hostname: new_hostname}},
        %{clients: clients, key_path: key_path} = state
      ) do
    if Default.validator_id() != validator_id do
      %{pid: old_pid} = Map.get(clients, validator_id)
      GenServer.stop(old_pid, :normal)
      {:ok, pid} = Client.start_link({new_hostname, @port, key_path})
      {:noreply, Map.put(clients, validator_id, %{pid: pid, hostname: new_hostname})}
    else
      {:noreply, state}
    end
  end

  def handle_info({"delete", validator_id}, %{clients: clients} = state) do
    if Default.validator_id() != validator_id do
      %{pid: pid} = Map.get(clients, validator_id)
      GenServer.stop(pid, :normal)
      {:noreply, Map.delete(clients, validator_id)}
    else
      # stop application
      Logger.info("Validator's licence deleted")
      System.stop(0)
      {:noreply, state}
    end
  end

  def handle_info(_, state) do
    {:noreply, state}
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
    PubSub.subscribe(@pubsub_server, "validator")
  end

  defp unsubscribe do
    PubSub.unsubscribe(@pubsub_server, "validator")
  end
end
