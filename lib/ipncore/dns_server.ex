defmodule Ipncore.DNS.Server do
  @moduledoc """
  DNS server based on `GenServer`.
  """

  @callback handle(DNS.Record.t(), {:inet.ip(), :inet.port()}) :: DNS.Record.t()

  defmacro __using__(_) do
    quote [] do
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
        IO.puts("DNS Server listening at #{Inet.to_str(ip)}:#{port}")

        # accept_loop(socket, handler)
        {:ok, %{port: port, socket: socket}}
      end

      def handle_info({:udp, client, ip, wtv, data}, state) do
        record = DNS.Record.decode(data)
        response = handle(record, client)
        Socket.Datagram.send!(state.socket, DNS.Record.encode(response), {ip, wtv})
        {:noreply, state}
      end
    end
  end
end
