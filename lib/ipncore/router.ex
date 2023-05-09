defmodule Ipncore.Router do
  alias Ippan.RequestHandler
  use Plug.Router
  # use Plug.ErrorHandler

  @json Application.compile_env(:ipncore, :json)

  plug(:match)
  plug(:dispatch)

  # require Logger
  get "/v1/call" do
    try do
      body = decode!(conn.params)

      case RequestHandler.handle(body) do
        {:ok, hash} ->
          json(conn, %{"hash" => Base.encode16(hash)})

        {:error, err_message} ->
          json(conn, 400, %{"status" => "error", "msg" => err_message})

        _err_message ->
          # Logger.error(inspect(err_message))
          send_resp(conn, 500, "")
      end
    rescue
      _e ->
        # Logger.info(Exception.format(:error, e, __STACKTRACE__))
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

  defp decode!(%{"_json" => body}) do
    case body do
      [type, timestamp, from, args, auth] ->
        {type, timestamp, from, args, Base.decode64!(auth)}

      _ ->
        raise RuntimeError, "Error decode request"
    end
  end

  defp json(conn, status, data) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, @json.encode!(data))
  end
end
