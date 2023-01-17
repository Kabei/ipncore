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
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(ip: ip, port: port) do
    socket = Socket.UDP.open!(port, as: :binary, mode: :active, local: [address: ip])
    IO.puts("DNS Server listening at UDP #{Inet.to_str(ip)}:#{port}")

    {:ok, %{port: port, socket: socket}}
  end

  def handle_info({:udp, client, ip, wtv, data}, state) do
    try do
      case Ipncore.DNS.handle(data, client) do
        nil ->
          :ok

        response ->
          Socket.Datagram.send!(state.socket, response, {ip, wtv})
      end
    rescue
      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
    end

    {:noreply, state}
  end
end
