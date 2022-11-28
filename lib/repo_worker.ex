defmodule Ipncore.RepoWorker do
  alias Ipncore.Repo
  alias __MODULE__
  use GenServer

  @impl true
  def init(stack) do
    {:ok, stack}
  end

  def start_link(_opts) do
    GenServer.start_link(RepoWorker, [], name: RepoWorker)
  end

  def run(nil), do: :ok

  def run(multi_or_fun) do
    GenServer.cast(RepoWorker, {:tx, multi_or_fun})
  end

  @impl true
  def handle_cast({:tx, multi}, state) do
    result = Repo.transaction(multi)
    IO.inspect(result)
    {:noreply, state}
  end
end
