defmodule Ipncore.Router do
  use Plug.Router
  require Logger

  @file_extension Application.compile_env(:ipncore, :block_file_ext)

  plug(:match)
  plug(:dispatch)

  get "/v1/download/block/:vid/:height" do
    data_dir = Application.get_env(:ipncore, :data_dir)
    block_path = Path.join([data_dir, "blocks", "#{vid}.#{height}.#{@file_extension}"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type("application/octet-stream")
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/v1/download/block/decoded/:vid/:height" do
    decode_dir = Application.get_env(:ipncore, :decode_dir)
    block_path = Path.join([decode_dir, "#{vid}.#{height}.#{@file_extension}"])

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
end
