defmodule Ipncore.WebSocketHandler do
  @behaviour :cowboy_websocket

  # terminate if no activity for one minute
  @timeout 120_000
  @max_frame_size 8_000_000
  @max_subcriptions 5
  # pubsub server name
  @pubsub_server :pubsub

  alias Phoenix.PubSub
  require Logger

  def init(req, _state) do
    IO.inspect(req)

    params = Plug.Conn.Query.decode(req.qs)
    IO.inspect(params)

    case params do
      %{"topic" => topic} ->
        state = %{subcriptions: 1}
        PubSub.subscribe(@pubsub_server, topic)

        {:cowboy_websocket, req, state,
         %{idle_timeout: @timeout, max_frame_size: @max_frame_size}}

      _ ->
        state = %{subcriptions: 0}

        {:cowboy_websocket, req, state,
         %{idle_timeout: @timeout, max_frame_size: @max_frame_size}}
    end
  end

  # Called on websocket connection initialization.
  def websocket_init(state) do
    {:ok, state, :hibernate}
  end

  # Handle 'ping' messages from the browser - reply
  def websocket_handle({:text, "ping"}, state) do
    {:ok, state, :hibernate}
  end

  # Handle other messages from the browser - don't reply
  def websocket_handle({:text, raw_message}, state) do
    case Jason.decode(raw_message) do
      {:ok, message} ->
        process(message, state)

      _ ->
        Logger.warn("Error json decode")
        {:stop, state}
    end
  end

  # Format and forward elixir messages to client
  def websocket_info(message, state) do
    {:reply, {:text, message}, state, :hibernate}
  end

  # No matter why we terminate, remove all of this pids subscriptions
  def terminate(_reason, _req, _state) do
    IO.puts("terminate")
    :ok
  end

  def push(topic, message) do
    case Jason.encode(message) do
      {:ok, response} -> PubSub.broadcast(@pubsub_server, topic, response)
      _ -> :error
    end
  end

  defp process(%{"subscribe" => topic, "id" => req_id}, %{subcriptions: subcriptions} = state) do
    subcriptions = subcriptions + 1

    cond do
      subcriptions > @max_subcriptions ->
        send_error(req_id, "Max subscriptions exceeded", state)

      true ->
        PubSub.subscribe(@pubsub_server, topic)
        new_state = Map.put(state, :subcriptions, subcriptions)

        send_ok(req_id, new_state)
    end
  end

  defp process(%{"unsubscribe" => topic, "id" => req_id}, %{subcriptions: subcriptions} = state) do
    subcriptions = subcriptions - 1

    cond do
      subcriptions < 0 ->
        {:ok, state, :hibernate}

      true ->
        PubSub.unsubscribe(@pubsub_server, topic)
        new_state = Map.put(state, :subcriptions, subcriptions)

        send_ok(req_id, new_state)
    end
  end

  defp process(_, state) do
    Logger.warn("Command not found")
    {:stop, state}
  end

  # defp send_message(message, state) do
  #   response = Jason.encode(message)
  #   {:reply, {:text, response}, state, :hibernate}
  # end

  defp send_ok(req_id, state) do
    response = Jason.encode!(%{id: req_id, status: "ok"})
    {:reply, {:text, response}, state, :hibernate}
  end

  defp send_error(req_id, message, state) do
    response = Jason.encode!(%{id: req_id, message: message, status: "ok"})
    {:reply, {:text, response}, state, :hibernate}
  end
end
