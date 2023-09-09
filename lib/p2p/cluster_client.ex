defmodule Ippan.ClusterClient do
  use GenServer, restart: :transient
  alias Ippan.P2P
  alias Ippan.Utils
  require Logger

  @id :name
  @adapter :gen_tcp
  @node Ippan.ClusterNode
  @ping_interval 45_000
  @opts Application.compile_env(:ipncore, :p2p_client)
  @time_to_reconnect 1_000

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
  def handle_continue(:init, state) do
    connect(state)
  end

  defp connect(
         %{id: node_id, hostname: hostname, port: port, net_pubkey: net_pubkey, opts: opts} =
           state
       ) do
    retry = Keyword.get(opts, :retry, 0)
    from_id = :persistent_term.get(@id)

    with {:ok, ip_addr} <- Utils.getaddr(hostname),
         {:ok, socket} <- @adapter.connect(ip_addr, port, @opts),
         false <- @node.alive?(node_id) do
      case P2P.client_handshake(socket, from_id, net_pubkey, :persistent_term.get(:privkey)) do
        {:ok, sharedkey} ->
          :ok = :inet.setopts(socket, active: true)

          map = %{
            socket: socket,
            sharedkey: sharedkey,
            hostname: hostname,
            net_pubkey: net_pubkey
          }

          new_state = Map.merge(state, map)
          @node.on_connect(node_id, map)
          {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
          {:noreply, Map.put(new_state, :tRef, tRef), :hibernate}

        error ->
          IO.inspect("ERROR 2")
          IO.inspect(error)
          retry_connect(state, retry, error)
      end
    else
      false ->
        IO.puts("[cluster] member already exists")
        # {:noreply, Map.merge(state, @node.info(node_id))}
        {:stop, :normal, state}

      error ->
        IO.inspect("ERROR")
        IO.inspect(error)
        retry_connect(state, retry, error)
    end
  end

  defp retry_connect(state, retry, error) do
    IO.inspect("retry #{inspect(error)}")

    cond do
      error == :halt ->
        IO.inspect(error)
        {:stop, :normal, state}

      retry == :infinity ->
        :timer.sleep(@time_to_reconnect)
        connect(state)

      retry > 0 ->
        :timer.sleep(@time_to_reconnect)
        opts = Keyword.put(state.opts, :retry, retry - 1)
        connect(%{state | opts: opts})

      true ->
        {:stop, :normal, state}
    end
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
    @node.on_disconnect(state)
    {:stop, state}
  end

  if Mix.env() == :dev do
    def handle_info({:tcp_error, _socket, reason}, %{id: id} = state) do
      Logger.debug("tcp_error #{reason} | #{id}")
      {:noreply, state}
    end
  end

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
  end

  def terminate(_reason, _state), do: :ok
end
