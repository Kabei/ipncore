defmodule Ipncore.Router do
  alias Ippan.RequestHandler
  use Plug.Router
  # use Plug.ErrorHandler

  @json Application.compile_env(:ipncore, :json)
  @max_size Application.compile_env(:ipncore, :message_max_size)

  plug(:match)
  plug(:dispatch)

  require Logger

  post "/v1/call" do
    try do
      {:ok, body, conn} = Plug.Conn.read_body(conn, length: @max_size)

      hash = Blake3.hash(body)

      case get_req_header(conn, "auth") do
        [sig] ->
          sig = Fast64.decode64(sig)
          size = byte_size(body) + byte_size(sig)
          RequestHandler.handle(hash, body, size, sig)

        _ ->
          size = byte_size(body)
          RequestHandler.handle(hash, body, size)
      end
      |> case do
        {:error, msg} ->
          send_resp(conn, 400, msg)

        _ ->
          json(conn, %{"hash" => Base.encode16(hash)})
      end
    rescue
      e ->
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
        send_resp(conn, 400, "")
    end
  end

  match _ do
    send_resp(conn, 404, "")
  end

  # defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
  #   send_resp(conn, conn.status, "Something went wrong")
  # end

  defp json(conn, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, @json.encode!(data))
  end

  # defp json(conn, status, data) do
  #   conn
  #   |> put_resp_content_type("application/json")
  #   |> send_resp(status, @json.encode!(data))
  # end

  # defp decode64!(nil), do: nil
  # defp decode64!([]), do: nil

  # defp decode64!(x) do
  #   Fast64.decode64(x)
  # end
end
