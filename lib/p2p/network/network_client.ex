defmodule Ippan.NetworkClient do
  use GenServer, restart: :transient
  alias Ippan.P2P
  alias Ippan.Utils
  require Logger

  @id :vid
  @adapter :gen_tcp
  @node Ippan.NetworkNodes
  @ping_interval 45_000
  @app Mix.Project.config()[:app]
  @opts Application.compile_env(@app, :p2p_client)
  @time_to_connect 5_000
  @time_to_reconnect 1_000
  @via :client

  def start_link(_, args) do
    start_link(args)
  end

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, hibernate_after: 2_000)
  end

  @impl true
  def init(args = %{opts: opts}) do
    if Keyword.get(opts, :async, false) do
      {:ok, args, {:continue, :init}}
    else
      case connect(args) do
        {:noreply, state, _} ->
          {:ok, state, :hibernate}

        stop ->
          stop
      end
    end
  end

  @impl true
  def handle_continue(:connect, state) do
    connect(state)
  end

  defp connect(
         %{
           id: node_id,
           hostname: hostname,
           port: port,
           net_pubkey: net_pubkey,
           opts: opts,
           pid: pid
         } =
           state
       ) do
    retry = Keyword.get(opts, :retry, 0)
    from_id = :persistent_term.get(@id)

    with {:ok, ip_addr} <- Utils.getaddr(hostname),
         {:ok, socket} <- @adapter.connect(ip_addr, port, @opts, @time_to_connect),
         false <- @node.alive?(node_id) do
      case P2P.client_handshake(socket, from_id, net_pubkey, :persistent_term.get(:privkey)) do
        {:ok, sharedkey} ->
          :ok = :inet.setopts(socket, active: true)

          new_state =
            state
            |> Map.take([:id, :hostname, :port, :net_pubkey, :opts, :pid])
            |> Map.put(:socket, socket)
            |> Map.put(:sharedkey, sharedkey)

          @node.on_connect(node_id, new_state, @via)
          {:ok, tRef} = :timer.send_after(@ping_interval, :ping)

          # callback
          callback(pid, :ok)

          {:noreply, Map.put(new_state, :tRef, tRef), :hibernate}

        error ->
          IO.inspect("ERROR 2 #{inspect(error)}")
          retry_connect(state, retry, error)
      end
    else
      true ->
        IO.puts("[Node] member already exists")
        callback(state[:pid], :ok)
        {:stop, :normal, state}

      error ->
        IO.inspect("ERROR")
        IO.inspect(error)
        retry_connect(state, retry, error)
    end
  end

  defp retry_connect(state, retry, error) do
    cond do
      error == :halt ->
        IO.inspect(error)
        callback(state[:pid], :error)
        {:stop, :normal, state}

      retry == :infinity ->
        :timer.sleep(@time_to_reconnect)
        connect(state)

      retry > 0 ->
        :timer.sleep(@time_to_reconnect)
        opts = Keyword.put(state.opts, :retry, retry - 1)
        connect(%{state | opts: opts})

      true ->
        callback(state[:pid], :error)
        {:stop, :normal, state}
    end
  end

  defp callback(nil, _message), do: nil

  defp callback(pid, message) do
    send(pid, message)
  end

  @impl true
  def handle_info(:ping, %{socket: socket} = state) do
    @adapter.send(socket, "PING")
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:noreply, Map.put(state, :tRef, tRef), :hibernate}
  end

  def handle_info({:tcp, _socket, packet}, state) do
    @node.on_message(packet, state)
    {:noreply, state}
  end

  def handle_info({:tcp_closed, socket}, %{id: id} = state) do
    Logger.debug("tcp_closed | #{id}")
    @adapter.close(socket)
    @node.on_disconnect(state, 1, @via)
    {:stop, :normal, state}
  end

  if Mix.env() == :dev do
    def handle_info({:tcp_error, _socket, reason}, %{id: id} = state) do
      Logger.debug("tcp_error #{reason} | #{id}")
      {:noreply, state}
    end
  end

  @impl true
  def terminate(_reason, %{tRef: tRef} = _state) do
    :timer.cancel(tRef)
    # @node.on_disconnect(state, 0, @via)
  end

  def terminate(_reason, _state), do: :ok
end
