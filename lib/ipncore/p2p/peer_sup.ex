defmodule Ippan.P2P.PeerSupervisor do
  use DynamicSupervisor

  def start_link(children) do
    DynamicSupervisor.start_link(__MODULE__, children, name: __MODULE__)
  end

  def start_child(args) do
    spec = %{
      id: nil,
      start: {Ippan.P2P.PeerClient, :start_link, [args]}
    }

    DynamicSupervisor.start_child(__MODULE__, {Ippan.P2P.PeerClient, args})
  end

  @impl true
  def init(init_arg) do
    DynamicSupervisor.init(
      strategy: :one_for_one,
      extra_arguments: [init_arg]
    )
  end
end
