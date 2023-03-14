defmodule Ipncore.DNS.Worker do
  use GenServer

  def start_link(_), do: GenServer.start_link(__MODULE__, nil)

  @impl true
  def init(_), do: {:ok, nil}

  @impl true
  def handle_call({:dns, socket, ip, port, data, state}, _from, _state) do
    result =
      case Ipncore.DNS.handle(data, socket) do
        nil ->
          :ok

        response ->
          :gen_udp.send(state.socket, ip, port, response)
      end

    {:reply, result, state}
  end
end