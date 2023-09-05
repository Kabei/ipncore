defmodule Ippan.Network do
  @callback on_connect(node_id :: term(), map :: map()) :: any()
  @callback on_disconnect(state :: term()) :: any()
  @callback on_message(packet :: term(), state :: term()) :: any()
  @callback connect(node :: term(), opts :: keyword()) :: term() | {:error, term()}
  @callback disconnect(state :: term()) :: :ok
  @callback fetch(id :: term()) :: map() | nil
  @callback info(node_id :: term()) :: map() | nil
  @callback list() :: [term()]
  @callback alive?(node :: term()) :: boolean()
  @callback count() :: pos_integer()
  @callback cast(node :: term(), message :: term) :: :ok | :disconnect
  @callback call(node :: term(), message :: term) :: {:ok, term()} | {:error, term()}
  @callback broadcast(message :: term()) :: :ok
  @callback broadcast(message :: term(), role :: binary()) :: :ok
  @callback broadcast_except(message :: term(), ids :: list()) :: :ok
  @callback handle_request(method :: binary(), data :: map(), state :: term()) :: term()
  @callback handle_message(event :: binary(), data :: term(), state :: term()) :: any()
  @optional_callbacks [broadcast: 2, broadcast_except: 2, handle_request: 3, handle_message: 3]

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      use GenServer
      import Ippan.P2P, only: [encode: 2, decode!: 2]
      alias Ippan.{Network, Utils}
      alias Phoenix.PubSub
      alias Ippan.P2P
      require SqliteStore

      @behaviour Ippan.Network

      @adapter :gen_tcp
      @module __MODULE__
      @otp_app opts[:app]
      @table opts[:table]
      @name opts[:name]
      @opts opts[:opts]
      @pubsub opts[:pubsub]
      @topic opts[:topic]
      @supervisor opts[:sup]
      @time_to_reconnect 1_000

      def start_link(args) do
        GenServer.start_link(@module, args, hibernate_after: 5_000, name: __MODULE__)
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

        {:ok, sup} = @supervisor.start_link([])

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

      @impl Network
      def on_connect(
            node_id,
            %{
              socket: _socket,
              sharedkey: _sharedkey,
              hostname: _hostname,
              net_pubkey: _net_pubkey
            } =
              map
          ) do
        unless alive?(node_id) do
          :ets.insert(@table, {node_id, map})
        end
      end

      @impl Network
      def on_disconnect(%{id: node_id, opts: opts}) do
        reconnect = Keyword.get(opts, :reconnect, false)

        if reconnect do
          [{_, node}] = :ets.lookup(@table, node_id)
          :ets.delete(@table, node_id)
          connect(node, opts)
        else
          :ets.delete(@table, node_id)
        end
      end

      @impl Network
      def on_message(packet, %{sharedkey: sharedkey} = state) do
        case decode!(packet, sharedkey) do
          %{"_id" => id, "data" => data} ->
            PubSub.local_broadcast(@pubsub, "call:#{id}", data)

          %{"_id" => id, "method" => method, "data" => data} ->
            response = handle_request(method, data, state)
            bin = encode(%{"_id" => id, "data" => response}, sharedkey)
            @adapter.send(state.socket, bin)

          %{"event" => event, "data" => data} ->
            handle_message(event, data, state)

          _ ->
            :ok
        end
      end

      @impl Network
      def connect(
            %{id: node_id, hostname: hostname, net_pubkey: net_pubkey} = node,
            opts \\ []
          ) do
        retry = Keyword.get(opts, :retry, 0)

        {:ok, ip_addr} = Utils.getaddr(hostname)
        port = Application.get_env(@otp_app, @name)[:port]
        {:ok, socket} = @adapter.connect(ip_addr, port, @opts)

        unless alive?(node_id) do
          case P2P.client_handshake(socket, node_id, net_pubkey, :persistent_term.get(:privkey)) do
            {:ok, sharedkey} ->
              {:ok, pid} =
                @supervisor.start_child(%{
                  conn: :persistent_term.get(:asset_conn),
                  stmts: :persistent_term.get(:asset_stmt),
                  id: node_id,
                  sharedkey: sharedkey,
                  socket: socket,
                  opts: opts
                })

              :gen_tcp.controlling_process(socket, pid)
              :ok = :inet.setopts(socket, active: true)

              :ets.insert(
                @table,
                {node_id,
                 %{
                   socket: socket,
                   sharedkey: sharedkey,
                   hostname: hostname,
                   net_pubkey: net_pubkey
                 }}
              )

            error ->
              cond do
                error == :halt ->
                  error

                retry == :infinity ->
                  :timer.sleep(@time_to_reconnect)
                  connect(node, opts)

                retry > 0 ->
                  :timer.sleep(@time_to_reconnect)
                  connect(node, Keyword.put(opts, :retry, retry - 1))

                true ->
                  error
              end
          end
        else
          {:ok, info(node_id)}
        end
      end

      @impl Network
      def disconnect(%{id: node_id} = state) do
        %{socket: socket} = info(node_id)
        :ets.delete(@table, node_id)
        @adapter.close(socket)
      end

      @impl Network
      def info(node_id) do
        case :ets.lookup(@table, node_id) do
          [{_, data}] -> data
          _ -> nil
        end
      end

      @impl Network
      def list do
        :ets.tab2list(@table)
      end

      @impl Network
      def alive?(node_id) do
        :ets.member(@table, node_id)
      end

      @impl Network
      def count do
        :ets.info(@table, :size)
      end

      @impl Network
      def cast(node_id, message) do
        case info(node_id) do
          %{sharedkey: sharedkey, socket: socket} ->
            @adapter.send(socket, encode(message, sharedkey))

          _ ->
            :disconnect
        end
      end

      @impl Network
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

      @impl Network
      def broadcast(message) do
        for {_, %{sharedkey: sharedkey, socket: socket}} <- list() do
          @adapter.send(socket, encode(message, sharedkey))
        end

        :ok
      end

      @impl Network
      def broadcast_except(message, ids) do
        Enum.each(list(), fn {id, %{sharedkey: sharedkey, socket: socket}} ->
          if id not in ids do
            @adapter.send(socket, encode(message, sharedkey))
          end
        end)
      end

      defoverridable on_connect: 2, on_disconnect: 1, on_message: 2
    end
  end
end
