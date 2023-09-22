defmodule Ipncore.Router do
  use Plug.Router
  require Logger

  @block_extension Application.compile_env(:ipncore, :block_extension)

  plug(:match)
  plug(:dispatch)

  get "/v1/download/block/:vid/:height" do
    data_dir = :persistent_term.get(:data_dir)
    block_path = Path.join([data_dir, "blocks", "#{vid}.#{height}.#{@block_extension}"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/v1/download/block/decoded/:vid/:height" do
    decode_dir = :persistent_term.get(:decode_dir)
    block_path = Path.join([decode_dir, "#{vid}.#{height}.#{@block_extension}"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/v1/download/save/:filename" do
    store_dir = :persistent_term.get(:save_dir)
    path = Path.join([store_dir, filename, ".zip"])

    if File.exists?(path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, path)
    else
      send_resp(conn, 404, "")
    end
  end

  match _ do
    send_resp(conn, 404, "")
  end
end
