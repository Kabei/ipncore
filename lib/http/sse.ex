defmodule SSE do
  require Logger
  alias Phoenix.PubSub
  import Plug.Conn, only: [chunk: 2, halt: 1, put_resp_header: 3, send_chunked: 2]
  @timeout 120_000

  def stream(conn, pubsub, topic, timeout \\ @timeout) do
    conn =
      conn
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("connection", "keep-alive")
      |> send_chunked(200)

    PubSub.subscribe(pubsub, topic)
    Process.flag(:trap_exit, true)

    {_, adapter} = conn.adapter
    socket = adapter.socket.socket
    transport = adapter.socket.transport_module

    transport.controlling_process(socket, self())
    transport.setopts(socket, [{:active, false}])

    loop(conn, pubsub, topic, timeout)
  end

  def shutdown(conn, pubsub, topic) do
    PubSub.unsubscribe(pubsub, topic)
    halt(conn)
  end

  defp loop(conn, pubsub, topic, timeout) do
    receive do
      {:cont, %{"status" => _} = message} ->
        Logger.debug(message)

        conn
        |> chunk("event: #{topic}\ndata: #{Jason.encode!(message)}\n\n")
        |> case do
          {:ok, conn} ->
            loop(conn, pubsub, topic, timeout)

          _error ->
            shutdown(conn, pubsub, topic)
        end

      {:halt, %{"status" => _status} = message} ->
        Logger.debug(message)

        conn
        |> chunk("event: #{topic}\ndata: #{Jason.encode!(message)}\n\n")
        |> case do
          {:ok, conn} ->
            send_close(conn, pubsub, topic, :end)

          _error ->
            shutdown(conn, pubsub, topic)
        end

      {:tcp_closed, _socket} ->
        Logger.debug("TCP closed")
        shutdown(conn, pubsub, topic)

      {:tcp_error, _socket, _reason} ->
        Logger.debug("TCP error")
        shutdown(conn, pubsub, topic)

      {:plug_conn, _msg} ->
        loop(conn, pubsub, topic, timeout)

      {:EXIT, _from, _reason} ->
        PubSub.unsubscribe(pubsub, topic)
        Process.exit(conn.owner, :normal)

      {:DOWN, _reference, :process, _pid, _type} ->
        PubSub.unsubscribe(pubsub, topic)

      other ->
        Logger.debug("other #{inspect(other)}")
        send_close(conn, pubsub, topic, :shutdown)
    after
      timeout ->
        send_close(conn, pubsub, topic, :timeout)
    end
  end

  defp send_close(conn, pubsub, topic, reason) do
    Logger.debug("close")
    PubSub.unsubscribe(pubsub, topic)

    conn
    |> chunk("event: close\ndata: #{reason}\n\n")
    |> case do
      {:ok, conn} ->
        halt(conn)

      _error ->
        halt(conn)
    end
  end
end
