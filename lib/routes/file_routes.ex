defmodule Ipncore.FileRoutes do
  use Plug.Router

  @app Mix.Project.config()[:app]
  @block_extension Application.compile_env(@app, :block_extension)
  @decode_extension Application.compile_env(@app, :decode_extension)
  @content_type "application/octet-stream"

  plug(:match)
  plug(:dispatch)

  get "/save/:filename" do
    save_dir = :persistent_term.get(:save_dir)
    path = Path.join([save_dir, filename])

    if File.regular?(path) do
      conn
      |> put_resp_content_type(@content_type)
      |> send_file(200, path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/db/:filename" do
    store_dir = :persistent_term.get(:store_dir)
    path = Path.join([store_dir, filename])

    if File.regular?(path) do
      conn
      |> put_resp_content_type(@content_type)
      |> send_file(200, path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/block/:vid/:height" do
    base_dir = :persistent_term.get(:block_dir)
    block_path = Path.join([base_dir, "#{vid}.#{height}.#{@block_extension}"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type(@content_type)
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  get "/decode/:vid/:height" do
    base_dir = :persistent_term.get(:decode_dir)
    block_path = Path.join([base_dir, "#{vid}.#{height}.#{@decode_extension}"])

    if File.exists?(block_path) do
      conn
      |> put_resp_content_type(@content_type)
      |> send_file(200, block_path)
    else
      send_resp(conn, 404, "")
    end
  end

  match _ do
    send_resp(conn, 404, "")
  end
end
