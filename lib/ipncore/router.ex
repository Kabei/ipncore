defmodule Ipncore.Router do
  alias Ippan.RequestHandler
  use Plug.Router
  # use Plug.ErrorHandler
  require Logger

  @json Application.compile_env(:ipncore, :json)
  @max_size Application.compile_env(:ipncore, :message_max_size)

  plug(:match)
  plug(:dispatch)

  post "/v1/call" do
    try do
      {:ok, body, conn} = Plug.Conn.read_body(conn, length: @max_size)

      hash = Blake3.hash(body)

      {event, msg} =
        case get_req_header(conn, "auth") do
          [sig] ->
            sig = Fast64.decode64(sig)
            size = byte_size(body) + byte_size(sig)
            RequestHandler.valid!(hash, body, size, sig, Default.validator_id())

          _ ->
            size = byte_size(body)
            RequestHandler.valid!(hash, body, size)
        end

      case event do
        %{deferred: false} ->
          EventChannel.push({"valid", msg})

        %{deferred: true} ->
          EventChannel.push({"valid_df", msg})
      end

      EventChannel.push({"valid", msg})
      json(conn, %{"hash" => Base.encode16(hash)})
    rescue
      e in [IppanError] ->
        send_resp(conn, 400, e.message)

      e ->
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
        send_resp(conn, 400, "Invalid operation")
    end
  end

  get "/v1/download/block/:vid/:height" do
    data_dir = Application.get_env(:ipncore, :data_dir)
    block_path = Path.join([data_dir, "blocks", "#{vid}.#{height}.erl"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/v1/download/block-decode/:vid/:height" do
    data_dir = Application.get_env(:ipncore, :data_dir)
    block_path = Path.join([data_dir, "blocks-decode", "#{vid}.#{height}.erl"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
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
end
