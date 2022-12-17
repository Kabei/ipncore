defmodule Ipncore.DNS.TcpServer do
  require Logger
  use GenServer

  def start_link([ip, port]) do
    GenServer.start_link(__MODULE__, [ip, port])
  end

  def init([ip, port]) do
    {:ok, socket} = :gen_tcp.listen(port, [:binary, active: true])
    send(self(), :accept)

    IO.puts("DNS Server listening at TCP #{Inet.to_str(ip)}:#{port}")
    {:ok, %{port: port, socket: socket}}
  end

  def handle_info(:accept, %{socket: socket} = state) do
    {:ok, _} = :gen_tcp.accept(socket)

    {:noreply, state}
  end

  def handle_info({:tcp, socket, data}, state) do
    try do
      record = DNS.Record.decode(data)
      response = Ipncore.DNS.handle(record, socket)

      :ok = :gen_tcp.send(socket, DNS.Record.encode(response))
    rescue
      e ->
        Logger.error(Exception.format(:error, e, __STACKTRACE__))
    catch
      _x ->
        IO.puts("Error DNS")
    end

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _}, state), do: {:stop, :normal, state}
  def handle_info({:tcp_error, _}, state), do: {:stop, :normal, state}
  def handle_info(_, state), do: {:noreply, state}
end
