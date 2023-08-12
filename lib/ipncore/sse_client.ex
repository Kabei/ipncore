defmodule SseClient do
  use GenServer
  require Logger

  def start(url, timeout \\ :infinity) do
    GenServer.start_link(__MODULE__, {url, timeout}, [])
  end

  @impl true
  def init({url, timeout}) do
    Logger.debug("Connecting to stream...")
    HTTPoison.get!(url, [], recv_timeout: timeout, stream_to: self(), hackney: [:insecure])
    {:ok, nil}
  end

  @impl true
  def handle_info(%HTTPoison.AsyncChunk{chunk: chunk}, _state) do
    case String.split(chunk, ~r/^event:\s|\ndata:\s|\nid:\s|\n\n$/, trim: true) do
      [_event, data] ->
        Jason.decode!(data)

      [_event, data, _id] ->
        Jason.decode!(data)

      _nil ->
        raise "Don't know how to parse received chunk: \"#{chunk}\""
    end

    {:noreply, nil}
  end

  # In addition to message chunks, we also may receive status changes etc.
  def handle_info(%HTTPoison.AsyncStatus{} = status, _state) do
    Logger.debug("Connection status: #{inspect(status)}")
    {:noreply, nil}
  end

  def handle_info(%HTTPoison.AsyncHeaders{} = headers, _state) do
    Logger.debug("Connection headers: #{inspect(headers)}")
    {:noreply, nil}
  end
end
