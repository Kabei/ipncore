defmodule TxWorker do
  use GenServer
  alias Ippan.TxHandler
  require Logger

  def start_link({id, args}) do
    GenServer.start_link(__MODULE__, args, name: {__MODULE__, id}, hibernate_after: 10_000)
  end

  @impl true
  def init(args) do
    Process.flag(:trap_exit, true)

    {:ok, args}
  end

  @impl true
  def handle_cast(
        {:tx, source = %{args: args, type: %{fun: fun, modx: module}, validator: validator}},
        state = %{cref: cref}
      ) do
    case apply(module, fun, [source | args]) do
      :error ->
        :counters.add(cref, 2, 1)

      {:error, _} ->
        :counters.add(cref, 2, 1)

      _ ->
        nil
    end

    {:noreply, state}
  end

  # def handle_cast(
  #       {:verify, source = %{args: args, type: %{fun: fun, mod: module}}},
  #       state = %{cref: cref}
  #     ) do
  #       tx = %{

  #       }

  #   TxHandler.valid?(tx)

  #   {:noreply, state}
  # end

  def handle_cast({:init, new_state}, state) do
    {:noreply, Map.merge(state, new_state)}
  end

  @impl true
  def handle_info({:EXIT, _pid, reason}, state = %{cref: cref}) do
    Logger.warning("#{inspect(reason)} in handle_info")
    :counters.add(cref, 1, 1)
    {:stop, reason, state}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
