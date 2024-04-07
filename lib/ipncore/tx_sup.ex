defmodule TxSupervisor do
  use DynamicSupervisor

  def start_link(args) do
    DynamicSupervisor.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_arg) do
    cpu = (System.schedulers_online() - 2) |> max(1)
    sup = DynamicSupervisor.init(strategy: :one_for_one)

    Enum.each(0..cpu, fn n ->
      DynamicSupervisor.start_child(__MODULE__, {TxWorker, %{num: n}})
    end)

    sup
  end
end
