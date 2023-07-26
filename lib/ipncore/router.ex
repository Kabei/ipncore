defmodule Ipncore.Router do
  alias Ippan.RequestHandler
  use Plug.Router
  # use Plug.ErrorHandler
  require Logger
  alias Phoenix.PubSub
  alias Ippan.Block
  require Global

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
            RequestHandler.valid!(hash, body, size, sig, Global.validator_id())

          _ ->
            size = byte_size(body)
            RequestHandler.valid!(hash, body, size)
        end

      case event do
        %{deferred: false} ->
          MessageStore.insert(msg)

          PubSub.direct_broadcast(
            Global.miner(),
            :verifiers,
            "event",
            {"valid", node(), hash, msg}
          )

        %{deferred: true} ->
          MessageStore.insert_df(msg)

          PubSub.direct_broadcast(
            Global.miner(),
            :verifiers,
            "event",
            {"valid_df", node(), hash, msg}
          )
      end

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
      miner = System.get_env("MINER")

      unless is_nil(miner) do
        ip_local = String.split(miner, "@") |> List.last()
        url = Block.cluster_block_url(ip_local, vid, height)

        case Curl.download_block(url, block_path) do
          :ok ->
            conn
            |> put_resp_content_type("application/octet-stream")
            |> send_file(200, block_path)

          res ->
            Logger.debug(inspect(res))
            send_resp(conn, 404, "")
        end
      else
        send_resp(conn, 404, "")
      end
    end
  end

  get "/v1/download/block-decode/:vid/:height" do
    decode_dir = Application.get_env(:ipncore, :decode_dir)
    block_path = Path.join([decode_dir, "#{vid}.#{height}.erl"])

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
