defmodule Ippan.P2P.Server do
  use ThousandIsland.Handler
  alias Ippan.P2P
  import Ippan.P2P, only: [decode!: 2, encode: 2]
  require Logger

  @adapter ThousandIsland.Socket

  @impl ThousandIsland.Handler
  def handle_connection(socket, state) do
    # Logger.debug("handle_connection #{inspect(state)}")
    P2P.server_handshake(socket, state, ValidatorStore)
  end

  @impl ThousandIsland.Handler
  def handle_data("PING", _socket, state) do
    {:continue, state}
  end

  # event data | id method data
  def handle_data(
        packet,
        socket,
        %{id: vid, pubkey: pubkey, sharedkey: sharedkey} = state
      ) do
    try do
      case decode!(packet, sharedkey) do
        %{"event" => event, "data" => data} ->
          from = %{id: vid, pubkey: pubkey}
          handle_event(event, from, data)

        %{"id" => id, "method" => method, "data" => data} ->
          case handle_request(method, data) do
            :not_found ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "error", "data" => 404}, sharedkey)
              )

            response ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "ok", "data" => response}, sharedkey)
              )
          end

        %{"id" => id, "method" => method} ->
          case handle_request(method) do
            :not_found ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "error", "data" => 404}, sharedkey)
              )

            response ->
              @adapter.send(
                socket,
                encode(%{"id" => id, "status" => "ok", "data" => response}, sharedkey)
              )
          end

        result ->
          Logger.warning(inspect(result))
          :ok
      end
    rescue
      e ->
        # Logger.error(inspect(e))
        Logger.debug(Exception.format(:error, e, __STACKTRACE__))
    catch
      :exit, msg ->
        Logger.debug(inspect(msg))
    end

    {:continue, state}
  end

  @impl ThousandIsland.Handler
  def handle_close(_socket, _state) do
    Logger.debug("handle close socket")

    :ok
  end

  @impl ThousandIsland.Handler
  def handle_shutdown(_socket, _state) do
    Logger.debug("handle shutdown")

    :ok
  end

  @impl ThousandIsland.Handler
  def handle_timeout(_socket, _state) do
    Logger.debug("handle timeout")
    :ok
  end

  defp handle_event("block_msg", from, data) do
    block = MapUtil.to_existing_atoms(data)
    send(VoteCounter, {"new_recv", from, true, block})
  end

  defp handle_event(_, _, _), do: :ok

  defp handle_request("get_state") do
    BlockTimer.get_state()
  end

  defp handle_request(_), do: :not_found

  defp handle_request("get_block_messages", %{"round" => _round}) do
    :ok
  end

  defp handle_request(_, _) do
    :not_found
  end
end
