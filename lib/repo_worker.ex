defmodule Ipncore.RepoWorker do
  alias Ipncore.Repo
  alias __MODULE__
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(RepoWorker, [], name: RepoWorker)
  end

  @impl true
  def init(stack) do
    {:ok, stack}
  end

  def run(nil), do: :none

  def run(%Ecto.Multi{} = multi_or_fun) do
    GenServer.cast(RepoWorker, {:tx, multi_or_fun})
  end

  def run(_), do: :none

  @impl true
  def handle_cast({:tx, multi}, state) do
    # sends a message back to the TaskRunner when completed
    Task.async(fn ->
      Repo.transaction(multi)
    end)

    {:noreply, state}
  end

  require Logger
  # handle_info/2 receives generic messages from the Task processes
  @impl true
  def handle_info({_task, {:ok, result}}, state) do
    Logger.info("#{inspect(result)} Job Done.")
    {:noreply, state}
  end

  def handle_info({_task, {:error, reason}}, state) do
    Logger.error("Failed to completed job: #{reason}")
    {:noreply, state}
  end

  def handle_info(_, state), do: {:noreply, state}
end
