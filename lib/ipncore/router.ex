defmodule Ipncore.Router do
  alias Ippan.RequestHandler
  use Plug.Router
  # use Plug.ErrorHandler

  @json Application.compile_env(:ipncore, :json)
  @max_size Application.compile_env(:ipncore, :message_max_size)
  @timeout Application.compile_env(:ipncore, :message_timeout)

  plug(:match)
  plug(:dispatch)

  # require Logger
  post "/v1/call" do
    try do
      {:ok, body, conn} = Plug.Conn.read_body(conn, length: @max_size)

      hash = Blake3.hash(body)
      sig = decode64!(get_req_header(conn, "auth"))
      size = byte_size(body) + byte_size(sig)

      msg =
        [_type, timestamp, _from, _args] =
        body
        |> Jsonrs.decode()

      cond do
        :os.timestamp() in (timestamp - @timeout)..(timestamp - @timeout) ->
          raise IppanError, "Invalid timestamp"

        true ->
          case sig do
            nil ->
              RequestHandler.handle(hash, msg, size)

            _ ->
              RequestHandler.handle(hash, msg, size, sig)
          end

          json(conn, %{"hash" => Base.encode16(hash)})
      end

      # case
      #   :ok ->

      #   {:error, err_message} ->
      #     json(conn, 400, %{"status" => "error", "msg" => err_message})

      #   _err_message ->
      #     # Logger.error(inspect(err_message))
      #     send_resp(conn, 500, "")
      # end
    rescue
      _e ->
        # Logger.debug(Exception.format(:error, e, __STACKTRACE__))
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

  # defp decode!() do
  #   case body do
  #     [type, timestamp, from, args, auth] ->
  #       {type, timestamp, from, args, Base.decode64!(auth)}

  #     _ ->
  #       raise RuntimeError, "Error decode request"
  #   end
  # end

  # defp json(conn, status, data) do
  #   conn
  #   |> put_resp_content_type("application/json")
  #   |> send_resp(status, @json.encode!(data))
  # end

  defp decode64!([]), do: nil
  defp decode64!(nil), do: nil

  defp decode64!(x) do
    Base.decode64!(x)
  end
end
