defmodule Ipncore.DNS.Server do
  @moduledoc """
  DNS server based on `GenServer`.
  """

  # @callback handle(DNS.Record.t(), {:inet.ip(), :inet.port()}) :: DNS.Record.t()
  require Logger
  use GenServer

  @doc """
  Start Ipncore.DNS.Server` server.
  ## Options
  * `:ip` - set the local IP address for the server
  * `:port` - set the port number for the server
  """
  def start_link([ip, port]) do
    GenServer.start_link(__MODULE__, [ip, port])
  end

  def init([ip, port]) do
    socket = Socket.UDP.open!(port, as: :binary, mode: :active, local: [address: ip])
    IO.puts("DNS Server listening at UDP #{Inet.to_str(ip)}:#{port}")

    {:ok, %{port: port, socket: socket}}
  end

  def handle_info({:udp, client, ip, wtv, data}, state) do
    try do
      response = Ipncore.DNS.handle(data, client)
      Socket.Datagram.send!(state.socket, DNS.Record.encode(response), {ip, wtv})
    rescue
      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
    end

    {:noreply, state}
  end
end
