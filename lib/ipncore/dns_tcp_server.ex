defmodule Ipncore.DNS.TcpServer do
  require Logger
  use GenServer

  # def start_link([ip, port]) do
  #   GenServer.start_link(__MODULE__, [ip, port])
  # end

  # def init([ip, port]) do
  #   {:ok, socket} = :gen_tcp.listen(port, [:binary, active: false])
  #   send(self(), :accept)

  #   IO.puts("DNS Server listening at TCP #{Inet.to_str(ip)}:#{port}")
  #   {:ok, %{port: port, socket: socket}}
  # end

  def start_link([ip, port]) do
    GenServer.start_link(__MODULE__, [ip, port])
  end

  def init([ip, port]) do
    send(self(), {:accept, ip, port})

    {:ok, %{ip: ip, port: port}}
  end

  def accept(ip, port) do
    {:ok, listen_socket} =
      :gen_tcp.listen(port, [:binary, packet: :raw, active: false, reuseaddr: true])

    IO.puts("DNS Server listening at TCP #{Inet.to_str(ip)}:#{port}")

    loop_acceptor(listen_socket)
  end

  defp loop_acceptor(listen_socket) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, socket} ->
        {:ok, pid} = GenServer.start(Ipncore.DNS.TcpHandler, socket)
        :gen_tcp.controlling_process(socket, pid)

      _ ->
        :error
    end

    loop_acceptor(listen_socket)
  end

  def handle_info({:accept, ip, port}, state) do
    spawn(fn ->
      accept(ip, port)
    end)

    {:noreply, state}
  end
end

defmodule Ipncore.DNS.TcpHandler do
  use GenServer
  require Logger

  def start_link(socket) do
    GenServer.start_link(__MODULE__, socket)
  end

  @impl true
  def init(socket) do
    :inet.setopts(socket, active: true)
    {:ok, %{socket: socket}}
  end

  # TCP callbacks
  def handle_info({:tcp, socket, data}, state) do
    Logger.info("Tcp: #{inspect(data)}")
    try do
      record = DNS.Record.decode(data)
      response = Ipncore.DNS.handle(record, socket)

      :ok = :gen_tcp.send(socket, DNS.Record.encode(response))
    rescue
      MatchError ->
        IO.puts("Error DNS MatchError")
        # Logger.error(Exception.format(:error, e, __STACKTRACE__))
        :error
    catch
      _x ->
        IO.puts("Error DNS")
    end

    # :inet.setopts(socket, active: :once)
    {:noreply, state}
  end

  @impl true
  def handle_info({:tcp_closed, _socket}, state) do
    Logger.info("Socket is closed")
    {:stop, {:shutdown, "Socket is closed"}, state}
  end

  @impl true
  def handle_info({:tcp_error, _socket, reason}, state) do
    Logger.error("Tcp error: #{inspect(reason)}")
    {:stop, {:shutdown, "Tcp error: #{inspect(reason)}"}, state}
  end
end
