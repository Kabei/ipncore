defmodule Ippan.NetworkSup do
  use DynamicSupervisor
  @module __MODULE__
  def start_link(children) do
    case Process.whereis(@module) do
      nil ->
        DynamicSupervisor.start_link(@module, children, name: @module)

      pid ->
        {:ok, pid}
    end
  end

  def start_child(args) do
    DynamicSupervisor.start_child(@module, {Ippan.NetworkClient, args})
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one, extra_arguments: [init_arg])
  end
end
