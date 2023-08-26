defmodule NetworkNode do
  use GenServer
  import Ippan.P2P, only: [encode: 2, decode!: 2]
  alias Ippan.Func.Validator
  alias Phoenix.PubSub
  alias Ippan.P2P
  require SqliteStore

  @behaviour NetworkBehaviour

  @adapter :gen_tcp
  @module __MODULE__
  @table :nw
  @port 5815
  @tcp_opts Application.compile_env(:ipncore, :p2p_client)
  @pubsub :network
  @topic "p2p"

  def start_link(args) do
    GenServer.start_link(@module, args, hibernate_after: 5_000)
  end

  @impl true
  def init(_args) do
    case :ets.whereis(@module) do
      :undefined ->
        :ets.new(@module, [
          :set,
          :named_table,
          :public,
          read_concurrency: true,
          write_concurrency: true
        ])

      ref ->
        ref
    end

    {:ok, sup} = Ippan.P2P.PeerSupervisor.start_link([])

    PubSub.subscribe(@pubsub, @topic)

    {:ok, %{sup: sup}, :hibernate}
  end

  @impl true
  def handle_info({node_id, msg}, state) do
    %{sharedkey: sharedkey, socket: socket} = info(node_id)
    @adapter.send(socket, encode(msg, sharedkey))
    {:noreply, state}
  end

  def handle_info(msg, state) do
    for %{sharedkey: sharedkey, socket: socket} <- list() do
      @adapter.send(socket, encode(msg, sharedkey))
    end

    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %{sup: sup}) do
    PubSub.unsubscribe(@pubsub, @topic)
    :ets.delete(@table)
    DynamicSupervisor.stop(sup)
  end

  @impl NetworkBehaviour
  def on_connect(
        node_id,
        %{socket: _socket, sharedkey: _sharedkey, hostname: _hostname, net_pubkey: _net_pubkey} =
          map
      ) do
    unless alive?(node_id) do
      :ets.insert(@table, {node_id, map})
    end
  end

  @impl NetworkBehaviour
  def on_disconnect(node_id) do
    :ets.delete(@table, node_id)
  end

  @impl NetworkBehaviour
  def on_message(node_id, socket, packet, sharedkey) do
    case decode!(packet, sharedkey) do
      %{"_id" => id, "data" => data} ->
        PubSub.local_broadcast(@pubsub, "call:#{id}", data)

      %{"_id" => id, "method" => method, "data" => data} ->
        response = handle_response(method, data)
        bin = encode(%{"_id" => id, "data" => response}, sharedkey)
        @adapter.send(socket, bin)

      msg ->
        handle_message(Map.put(msg, "from", node_id))
    end
  end

  @impl NetworkBehaviour
  def connect(
        %{id: node_id, hostname: hostname, net_pubkey: net_pubkey} = node,
        opts \\ []
      ) do
    retry = Keyword.get(opts, :retry, 0)
    reconnect = Keyword.get(opts, :reconnect, 0)

    {:ok, ip_addr} = :inet_udp.getaddr(String.to_charlist(hostname))
    {:ok, socket} = @adapter.connect(ip_addr, @port, @tcp_opts)

    unless alive?(node_id) do
      case P2P.client_handshake(socket, node_id, net_pubkey, :persistent_term.get(:privkey)) do
        {:ok, sharedkey} ->
          {:ok, pid} =
            Ippan.P2P.PeerSupervisor.start_child(%{
              adapter: @adapter,
              mod: @module,
              id: node_id,
              sharedkey: sharedkey
            })

          :gen_tcp.controlling_process(socket, pid)
          :ok = :inet.setopts(socket, active: true)

          :ets.insert(
            @table,
            {node_id,
             %{socket: socket, sharedkey: sharedkey, hostname: hostname, net_pubkey: net_pubkey}}
          )

        error ->
          cond do
            error == :halt ->
              error

            reconnect > 0 ->
              :timer.sleep(reconnect)
              connect(node, opts)

            retry > 0 ->
              connect(node, Keyword.put(opts, :retry, retry - 1))

            true ->
              error
          end
      end
    else
      {:ok, info(node_id)}
    end
  end

  @impl NetworkBehaviour
  def disconnect(node_id) do
    %{socket: socket} = info(node_id)
    :ets.delete(@table, node_id)
    @adapter.close(socket)
  end

  @impl NetworkBehaviour
  def fetch(id) do
    SqliteStore.lookup_map(
      :validator,
      :persistent_term.get(:asset_conn),
      :persistent_term.get(:asset_stmt),
      "get_validator",
      id,
      Validator
    )
  end

  @impl NetworkBehaviour
  def info(node_id) do
    case :ets.lookup(@table, node_id) do
      [{_, data}] -> data
      _ -> nil
    end
  end

  @impl NetworkBehaviour
  def list do
    :ets.tab2list(@table)
  end

  @impl NetworkBehaviour
  def alive?(node_id) do
    :ets.member(@table, node_id)
  end

  @impl NetworkBehaviour
  def cast(node_id, message) do
    case info(node_id) do
      %{sharedkey: sharedkey, socket: socket} ->
        @adapter.send(socket, encode(message, sharedkey))

      _ ->
        :disconnect
    end
  end

  @impl NetworkBehaviour
  def call(node_id, method, data \\ %{}, timeout \\ 10_000, retry \\ 0) do
    id = :rand.bytes(10)
    topic = "call:#{id}"
    message = %{"_id" => id, "method" => method, "data" => data}
    %{sharedkey: sharedkey, socket: socket} = info(node_id)

    PubSub.subscribe(@pubsub, topic)
    @adapter.send(socket, encode(message, sharedkey))

    receive do
      result ->
        {:ok, result}
    after
      timeout ->
        if retry == 0 do
          PubSub.unsubscribe(@pubsub, topic)
          {:error, :timeout}
        else
          call_retry(socket, message, sharedkey, topic, timeout, retry - 1)
        end
    end
  end

  defp call_retry(socket, message, sharedkey, topic, timeout, retry) do
    @adapter.send(socket, encode(message, sharedkey))

    receive do
      result ->
        {:ok, result}
    after
      timeout ->
        if retry == 0 do
          PubSub.unsubscribe(@pubsub, topic)
          {:error, :timeout}
        else
          call_retry(socket, message, sharedkey, topic, timeout, retry - 1)
        end
    end
  end

  @impl NetworkBehaviour
  def broadcast(message) do
    for {_, %{sharedkey: sharedkey, socket: socket}} <- list() do
      @adapter.send(socket, encode(message, sharedkey))
    end

    :ok
  end

  @impl NetworkBehaviour
  def broadcast_except(message, ids) do
    Enum.each(list(), fn {id, %{sharedkey: sharedkey, socket: socket}} ->
      if id not in ids do
        @adapter.send(socket, encode(message, sharedkey))
      end
    end)
  end

  @impl NetworkBehaviour
  def handle_response(_method, _data) do
    %{}
  end

  @impl NetworkBehaviour
  def handle_message(_message) do
    :ok
  end
end
