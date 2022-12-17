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
      :gen_tcp.listen(port, [:binary, packet: :raw, active: :once, reuseaddr: true])

    IO.puts("DNS Server listening at TCP #{Inet.to_str(ip)}:#{port}")

    loop_acceptor(listen_socket)
  end

  defp loop_acceptor(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket)

    {:ok, pid} = GenServer.start(Ipncore.DNS.TcpClient, socket)

    :gen_tcp.controlling_process(socket, pid)
    loop_acceptor(listen_socket)
  end

  def handle_info({:accept, ip, port}, %{socket: socket} = state) do
    spawn(fn ->
      accept(ip, port)
    end)

    {:noreply, state}
  end
end

defmodule Ipncore.DNS.TcpClient do
  use GenServer

  def start_link(socket, opts \\ []) do
    GenServer.start_link(__MODULE__, socket, opts)
  end

  def init(socket) do
    %{socket: socket}
  end

  def send(pid, data) do
    GenServer.cast(pid, {:send, data})
  end

  # TCP callbacks

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
      MatchError ->
        IO.puts("Error DNS MatchError")
        # Logger.error(Exception.format(:error, e, __STACKTRACE__))
    catch
      _x ->
        IO.puts("Error DNS")
    end

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _socket}, state) do
    Process.exit(self(), :normal)
  end

  def handle_info({:tcp_error, _}, state), do: {:stop, :normal, state}

  def handle_info(_, state), do: {:noreply, state}

  # GenServer callbacks

  def handle_cast({:send, data}, %{socket: socket} = state) do
    :gen_tcp.send(socket, data)
    {:noreply, state}
  end
end
