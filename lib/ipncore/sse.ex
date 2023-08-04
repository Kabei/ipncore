defmodule Ipncore.SSE do
  require Logger
  alias Phoenix.PubSub
  import Plug.Conn, only: [chunk: 2, halt: 1, put_resp_header: 3, send_chunked: 2]
  @pubsub_server :events
  @timeout 120_000

  def stream(conn, topic, timeout \\ @timeout) do
    conn =
      conn
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("connection", "keep-alive")
      |> send_chunked(200)

    subscribe(topic)
    Process.flag(:trap_exit, true)

    {_, adapter} = conn.adapter
    socket = adapter.socket.socket
    transport = adapter.socket.transport_module

    transport.controlling_process(socket, self())
    transport.setopts(socket, [{:active, :once}])

    loop(conn, topic, timeout)
  end

  def shutdown(conn, topic) do
    unsubscribe(topic)
    halt(conn)
  end

  defp loop(conn, topic, timeout) do
    receive do
      {:cont, %{"status" => _} = message} ->
        Logger.debug(message)

        conn
        |> chunk("event: message\ndata: #{Jason.encode!(message)}\n\n")
        |> case do
          {:ok, conn} ->
            loop(conn, topic, timeout)

          _error ->
            shutdown(conn, topic)
        end

      {:halt, %{"status" => _status} = message} ->
        Logger.debug(message)

        conn
        |> chunk("event: message\ndata: #{Jason.encode!(message)}\n\n")
        |> case do
          {:ok, conn} ->
            send_close(conn, topic, :end)

          _error ->
            shutdown(conn, topic)
        end

      {:tcp_closed, _socket} ->
        Logger.debug("TCP closed")
        shutdown(conn, topic)

      {:tcp_error, _socket, _reason} ->
        Logger.debug("TCP error")
        shutdown(conn, topic)

      {:plug_conn, _msg} ->
        loop(conn, topic, timeout)

      {:EXIT, _from, _reason} ->
        unsubscribe(topic)
        Process.exit(conn.owner, :normal)

      {:DOWN, _reference, :process, _pid, _type} ->
        unsubscribe(topic)
        nil

      other ->
        Logger.debug("other #{inspect(other)}")
        send_close(conn, topic, :unknow)
    after
      timeout ->
        send_close(conn, topic, :timeout)
    end
  end

  defp send_close(conn, topic, reason) do
    Logger.debug("close")
    unsubscribe(topic)

    conn
    |> chunk("event: close\ndata: #{reason}\n\n")
    |> case do
      {:ok, conn} ->
        halt(conn)

      _error ->
        halt(conn)
    end
  end

  defp subscribe(topic) do
    :ok = PubSub.subscribe(@pubsub_server, topic)
  end

  defp unsubscribe(topic) do
    :ok = PubSub.unsubscribe(@pubsub_server, topic)
  end
end
