defmodule Curl do
  @max_file_size Application.compile_env(:ipncore, :block_max_size) |> to_string()

  def download(url, output) do
    case System.cmd("curl", [
           "--fail",
           "-k",
           "-L",
           "-s",
           url,
           "-o",
           output
         ]) do
      {_, 0} ->
        :ok

      {_, n} ->
        {:error, n}
    end
  end

  def download_block(url, output) do
    case System.cmd("curl", [
           "--fail",
           "-k",
           "-L",
           "-s",
           "--max-filesize",
           @max_file_size,
           url,
           "-o",
           output
         ]) do
      {_, 0} ->
        :ok

      {_, n} ->
        {:error, n}
    end
  end
end
