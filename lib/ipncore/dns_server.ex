defmodule Ipncore.DNS.Server do
  @moduledoc """
  DNS server based on `GenServer`.
  """

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
    {:ok, socket} = :gen_udp.open(port, [:binary, active: true, ip: ip, reuseaddr: true])

    IO.puts("DNS Server listening at UDP #{Inet.to_str(ip)}:#{port}")

    {:ok, %{port: port, socket: socket}}
  end

  def handle_info({:udp, socket, ip, port, data}, state) do
    spawn(fn ->
      :poolboy.transaction(
        :dns_worker,
        fn pid -> GenServer.call(pid, {:dns, socket, ip, port, data}) end,
        5_000
      )
    end)

    {:noreply, state}
  end
end
