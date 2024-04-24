defmodule TxSupervisor do
  use Supervisor

  def start_link(args) do
    Supervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @partitions (System.schedulers_online() - 1) |> max(1)
  def partitions do
    @partitions
  end

  @impl true
  def init(_arg) do
    children =
      Enum.map(0..@partitions, fn id ->
        {TxWorker, %{id: id}}
      end)

    Supervisor.init(children, strategy: :one_for_one)
  end
end
