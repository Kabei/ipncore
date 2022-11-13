defmodule Ipncore.StreamDeliver do
  import Plug.Conn

  @max_file_size Application.compile_env(:ipncore, :max_file_size)

  defp posts_path, do: Application.get_env(:ipncore, :post_path)

  def serve_video(conn, url, headers) do
    hash =
      :crypto.hash(:md5, url)
      |> Base.encode16()

    path = [posts_path(), hash] |> Path.join()

    case verify_file(path) do
      false ->
        case get_video_dash(url, hash) do
          {:ok, path} ->
            conn
            |> merge_resp_headers(headers)
            |> send_file(200, path)

          _ ->
            send_resp(conn, 404, "Not found")
        end

      true ->
        conn
        |> merge_resp_headers(headers)
        |> send_file(200, path)
    end
  end

  defp verify_file(path) do
    case File.stat(path) do
      {:error, _} ->
        false

      {:ok, %{size: 0}} ->
        File.rm(path)
        false

      {:ok, %{type: :regular}} ->
        true

      _ ->
        false
    end
  end

  defp get_video_dash(url, hash) do
    file_path =
      [posts_path(), hash]
      |> Path.join()

    Download.from(url,
      path: file_path,
      max_file_size: @max_file_size
    )
  end
end
