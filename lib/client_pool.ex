defmodule Ippan.P2P.ClientPool do
  require Logger
  use GenServer
  alias Ippan.P2P.Client
  alias Phoenix.PubSub

  @port Application.compile_env(:ipncore, :port, 5815)
  @pubsub_server :pubsub

  @impl true
  def init(_) do
    Process.flag(:trap_exit, true)
    validators = ValidatorStore.all()

    clients =
      for validator <- validators, into: %{} do
        {:ok, pid} = Client.start_link({validator.hostname, @port, nil})
        {validator.id, pid}
      end

    subscribe()

    {:ok, %{clients: clients}}
  end

  @impl true
  def handle_info({:EXIT, _pid, {:exit_trap, reason}}, state) do
    {:stop, reason, state}
  end

  def handle_info(msg, %{clients: clients} = state) do
    case msg do
      %{event: "validator.new", data: validator} ->
        {:ok, pid} = Client.start_link({validator.hostname, @port, nil})

        new_state = Map.put(clients, validator.id, pid)
        {:noreply, new_state}

      %{event: "validator.delete", data: validator} ->
        case Map.get(clients, validator.id) do
          nil ->
            {:noreply, state}

          pid ->
            new_state = Map.delete(clients, validator.id)
            GenServer.stop(pid, :normal)
            {:noreply, new_state}
        end

      _ ->
        {:noreply, state}
    end
  end

  @impl true
  def terminate(_reason, state) do
    unsubscribe()
    unlink(state)
    :ok
  end

  defp unlink(%{clients: clients}) do
    for {_id, pid} <- clients do
      GenServer.stop(pid, :normal)
    end
  end

  defp subscribe do
    PubSub.subscribe(@pubsub_server, "validator.new")
    PubSub.subscribe(@pubsub_server, "validator.delete")
  end

  defp unsubscribe do
    PubSub.unsubscribe(@pubsub_server, "validator.new")
    PubSub.unsubscribe(@pubsub_server, "validator.delete")
  end
end
