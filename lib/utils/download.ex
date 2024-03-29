defmodule Download do
  @doc """
  subj.

  Returns:

  * `:ok` if everything were ok.
  * `{ :error, :file_size_is_too_big }` if file size exceeds `max_file_size`
  * `{ :error, :download_failure }` if host isn't reachable
  * `{ :error, :eexist }` if file exists already

  Options:

    * `max_file_size` - max available file size for downloading (in bytes). Default is `1024 * 1024 * 1000` (1GB)
    * `path` - absolute file path for the saved file. Default is `pwd <> requested file name`

  ## Examples

      iex> Download.from("http://speedtest.ftp.otenet.gr/files/test100k.db")
      { :ok, "/absolute/path/to/test_100k.db" }

      iex> Download.from("http://speedtest.ftp.otenet.gr/files/test100k.db", [max_file_size: 99 * 1000])
      { :error, :file_size_is_too_big }

      iex> Download.from("http://speedtest.ftp.otenet.gr/files/test100k.db", [path: "/custom/absolute/file/path.db"])
      { :ok, "/custom/absolute/file/path.db" }

  """

  # 1 GB
  @max_file_size 1024 * 1024 * 1000
  @timeout 60_000
  @module __MODULE__

  def from(url, path, max_file_size \\ @max_file_size, timeout \\ @timeout) do
    with {:ok, file} <- create_file(path),
         {:ok, response_parsing_pid} <- create_process(file, path, max_file_size, timeout),
         {:ok, _pid} <- start_download(url, response_parsing_pid, path),
         :ok <- wait_for_download(),
         do: :ok
  end

  defp create_file(path), do: File.open(path, [:write, :exclusive])

  defp create_process(file, path, max_file_size, timeout) do
    opts = %{
      file: file,
      controlling_pid: self(),
      path: path,
      max_file_size: max_file_size,
      downloaded_content_length: 0,
      timeout: timeout
    }

    {:ok, spawn_link(@module, :do_download, [opts])}
  end

  defp start_download(url, response_parsing_pid, path) do
    request = HTTPoison.get(url, %{}, stream_to: response_parsing_pid, hackney: [:insecure])

    case request do
      {:error, _reason} ->
        File.rm!(path)

      _ ->
        nil
    end

    request
  end

  defp wait_for_download do
    receive do
      reason -> reason
    end
  end

  alias HTTPoison.{AsyncHeaders, AsyncStatus, AsyncChunk, AsyncEnd}
  require Logger
  @doc false
  def do_download(opts) do
    receive do
      response_chunk ->
        handle_async_response_chunk(response_chunk, opts)
    after
      opts.timeout -> {:error, :timeout}
    end
  end

  defp handle_async_response_chunk(%AsyncStatus{code: 200}, opts), do: do_download(opts)

  defp handle_async_response_chunk(%AsyncStatus{code: status_code}, opts) do
    finish_download({:error, :unexpected_status_code, status_code}, opts)
  end

  defp handle_async_response_chunk(%AsyncHeaders{headers: headers}, opts) do
    content_length_header =
      Enum.find(headers, fn {header_name, _value} ->
        header_name == "Content-Length"
      end)

    do_handle_content_length(content_length_header, opts)
  end

  defp handle_async_response_chunk(%AsyncChunk{chunk: data}, opts) do
    downloaded_content_length = opts.downloaded_content_length + byte_size(data)

    if downloaded_content_length < opts.max_file_size do
      IO.binwrite(opts.file, data)

      opts_with_content_length_increased =
        Map.put(opts, :downloaded_content_length, downloaded_content_length)

      do_download(opts_with_content_length_increased)
    else
      finish_download({:error, :file_size_is_too_big}, opts)
    end
  end

  defp handle_async_response_chunk(%AsyncEnd{}, opts), do: finish_download(:ok, opts)

  defp handle_async_response_chunk(%HTTPoison.Error{reason: reason}, opts),
    do: finish_download({:error, reason}, opts)

  # Uncomment one line below if you are prefer to test not "Content-Length" header response, but a real file size
  # defp do_handle_content_length(_, opts), do: do_download(opts)
  defp do_handle_content_length({"Content-Length", content_length}, opts) do
    if String.to_integer(content_length) > opts.max_file_size do
      finish_download({:error, :file_size_is_too_big}, opts)
    else
      do_download(opts)
    end
  end

  defp do_handle_content_length(nil, opts), do: do_download(opts)

  defp finish_download(reason, opts) do
    File.close(opts.file)

    if reason != :ok do
      File.rm!(opts.path)
    end

    send(opts.controlling_pid, reason)
  end
end

defmodule DownloadTask do
  use GenServer

  @max_file_size 1024 * 1024 * 1000
  @timeout 60_000
  @module __MODULE__

  def start_link do
    GenServer.start_link(@module, [], [])
  end

  @impl true
  def init(_) do
    {:ok, nil}
  end

  def start(url, path, max_file_size \\ @max_file_size, timeout \\ @timeout) do
    {:ok, pid} = start_link()

    :gen_server.call(pid, {:download, url, path, max_file_size, timeout}, :infinity)
  end

  @impl true
  def handle_call({:download, url, path, max_file_size, timeout}, _from, state) do
    result = Download.from(url, path, max_file_size, timeout)
    {:stop, :normal, result, state}
  end
end
