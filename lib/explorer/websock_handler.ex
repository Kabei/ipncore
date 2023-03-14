defmodule Ipncore.WebSockHandler do
  @moduledoc false

  @behaviour WebSock

  alias Phoenix.PubSub
  require Logger

  @pubsub_server :pubsub
  @max_subcriptions 5

  @impl true
  def init(%{"topic" => topic}) do
    PubSub.subscribe(@pubsub_server, topic)
    {:ok, %{subcriptions: 1}}
  end

  def init(_) do
    {:ok, %{subcriptions: 0}}
  end

  @impl true
  def handle_in({"PING", _}, state) do
    {:ok, state}
  end

  def handle_in({data, _}, state) do
    case Jason.decode(data) do
      {:ok, message} ->
        process(message, state)

      _ ->
        Logger.debug("Error json decode")
        {:stop, {:error, 419}, state}
    end
  end

  @impl true
  def handle_info(msg, state) when is_binary(msg) do
    {:push, {:text, msg}, state}
  end

  def handle_info(msg, state) do
    {:push, {:text, Jason.encode!(msg)}, state}
  end

  @impl true
  def terminate(reason, _state) do
    IO.inspect("terminate: #{reason}")
    :ok
  end

  # op
  def push(topic, message) do
    case Jason.encode(message) do
      {:ok, response} -> PubSub.broadcast(@pubsub_server, topic, response)
      _ -> :error
    end
  end

  defp process(
         %{"action" => "subscribe", "topic" => topic, "ref" => ref},
         %{subcriptions: subcriptions} = state
       ) do
    subcriptions = subcriptions + 1

    cond do
      subcriptions > @max_subcriptions ->
        send_error(ref, "Max subscriptions exceeded", state)

      true ->
        PubSub.subscribe(@pubsub_server, topic)
        new_state = Map.put(state, :subcriptions, subcriptions)

        send_ok(ref, new_state)
    end
  end

  defp process(
         %{"action" => "unsubscribe", "topic" => topic, "ref" => ref},
         %{subcriptions: subcriptions} = state
       ) do
    subcriptions = subcriptions - 1

    cond do
      subcriptions < 0 ->
        {:ok, state}

      true ->
        PubSub.unsubscribe(@pubsub_server, topic)
        new_state = Map.put(state, :subcriptions, subcriptions)

        send_ok(ref, new_state)
    end
  end

  defp process(_, state) do
    Logger.debug("Command not found")
    {:stop, {:error, 404}, state}
  end

  defp send_ok(ref, state) do
    response = Jason.encode!(%{ref: ref, status: "ok"})
    {:push, {:text, response}, state}
  end

  defp send_error(ref, message, state) do
    response = Jason.encode!(%{ref: ref, message: message, status: "ok"})
    {:push, {:text, response}, state}
  end
end
