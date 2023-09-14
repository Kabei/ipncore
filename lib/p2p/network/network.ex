defmodule Ippan.Network do
  require Logger
  @callback on_connect(node_id :: term(), map :: map()) :: any()
  @callback on_disconnect(state :: term()) :: any()
  @callback on_message(packet :: term(), state :: term()) :: any()
  @callback connect(node :: term(), opts :: keyword()) :: term() | {:error, term()}
  @callback connect_async(node :: term(), opts :: keyword()) :: any()
  @callback disconnect(state :: term()) :: :ok
  @callback fetch(id :: term()) :: map() | nil
  @callback info(node_id :: term()) :: map() | nil
  @callback list() :: [term()]
  @callback alive?(node :: term()) :: boolean()
  @callback count() :: pos_integer()
  @callback cast(node :: term(), message :: term) :: :ok | :disconnect
  @callback call(
              node_id :: term(),
              method :: binary,
              message :: term,
              timeout :: integer(),
              retry :: integer()
            ) :: {:ok, term()} | {:error, term()}
  @callback broadcast(message :: term()) :: :ok
  @callback broadcast(message :: term(), role :: binary()) :: :ok
  @callback broadcast_except(message :: term(), ids :: list()) :: :ok
  @callback handle_request(method :: binary(), data :: map(), state :: term()) :: term()
  @callback handle_message(event :: binary(), data :: term(), state :: term()) :: any()
  # nodes
  @callback add_node(node :: term()) :: term()
  @callback update_node(node_id :: term(), args :: term()) :: term()
  @callback delete_node(node_id :: term()) :: term()

  @optional_callbacks [
    broadcast: 2,
    broadcast_except: 2,
    handle_request: 3,
    handle_message: 3,
    add_node: 1,
    update_node: 2,
    delete_node: 1
  ]

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      use GenServer
      import Ippan.P2P, only: [encode: 2, decode!: 2]
      alias Ippan.{Network, Utils}
      alias Phoenix.PubSub
      alias Ippan.P2P
      require SqliteStore
      require Logger

      @behaviour Ippan.Network

      @adapter :gen_tcp
      @module __MODULE__
      @otp_app opts[:app]
      @table opts[:table]
      @name opts[:name]
      @pubsub opts[:pubsub]
      @topic opts[:topic]
      @supervisor opts[:sup]
      @default_connect_opts opts[:conn_opts] || []

      def start_link(args) do
        GenServer.start_link(@module, args, hibernate_after: 5_000, name: __MODULE__)
      end

      @impl true
      def init(_args) do
        case :ets.whereis(@table) do
          :undefined ->
            :ets.new(@table, [
              :set,
              :named_table,
              :public,
              read_concurrency: false,
              write_concurrency: false
            ])

          ref ->
            ref
        end

        {:ok, sup} = @supervisor.start_link([])

        PubSub.subscribe(@pubsub, @topic)

        {:ok, %{sup: sup}, {:continue, :init}}
      end

      @impl true
      def handle_continue(:init, state) do
        on_init(state)
        {:noreply, state, :hibernate}
      end

      def on_init(_), do: :ok

      # @impl true
      # def handle_info({node_id, msg}, state) do
      #   %{sharedkey: sharedkey, socket: socket} = info(node_id)
      #   @adapter.send(socket, encode(msg, sharedkey))
      #   {:noreply, state}
      # end

      # def handle_info(msg, state) do
      #   for %{sharedkey: sharedkey, socket: socket} <- list() do
      #     @adapter.send(socket, encode(msg, sharedkey))
      #   end

      #   {:noreply, state}
      # end

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
      def on_disconnect(%{id: node_id, socket: socket, opts: opts} = state) do
        with [{_, node}] <- :ets.lookup(@table, node_id) do
          if socket == node.socket do
            :ets.delete(@table, node_id)
          end
        end

        if Keyword.get(opts, :reconnect, false) do
          connect(state, opts)
        end
      end

      def on_disconnect(%{id: node_id}) do
        :ets.delete(@table, node_id)
      end

      def on_disconnect(_), do: :ok

      @impl Network
      def on_message(packet, %{sharedkey: sharedkey} = state) do
        case decode!(packet, sharedkey) do
          # Receive question
          %{"_id" => id, "method" => method, "data" => data} ->
            # send answer
            try do
              response = handle_request(method, data, state)
              bin = encode(%{"_id" => id, "data" => response}, sharedkey)
              @adapter.send(state.socket, bin)
            rescue
              x ->
                bin = encode(%{"_id" => id, "data" => ["error", "Unknown"]}, sharedkey)
                @adapter.send(state.socket, encode(bin, sharedkey))
                Logger.error(x.message)
            end

          # Receive answer
          %{"_id" => id, "data" => data} ->
            PubSub.local_broadcast(@pubsub, "call:#{id}", data)

          # Event message (no return answer)
          %{"event" => event, "data" => data} ->
            handle_message(event, data, state)

          m ->
            Logger.debug(m)
            :ok
        end
      end

      @impl Network
      def connect(
            %{id: node_id, hostname: hostname, port: port, net_pubkey: net_pubkey} = node,
            opts \\ @default_connect_opts
          ) do
        @supervisor.start_child(
          Map.merge(node, %{
            conn: :persistent_term.get(:asset_conn),
            stmts: :persistent_term.get(:asset_stmt),
            opts: opts
          })
        )
      end

      @impl Network
      def connect_async(node, opts \\ @default_connect_opts) do
        connect(node, Keyword.put(opts, :async, true))
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
        id = :rand.bytes(8)
        topic = "call:#{id}"
        message = %{"_id" => id, "method" => method, "data" => data}

        case info(node_id) do
          nil ->
            {:error, :not_exists}

          %{sharedkey: sharedkey, socket: socket} ->
            PubSub.subscribe(@pubsub, topic)
            call_retry(socket, message, sharedkey, topic, timeout, retry)
        end
      end

      defp call_retry(socket, message, sharedkey, topic, timeout, retry) do
        @adapter.send(socket, encode(message, sharedkey))

        receive do
          ["error", error] ->
            {:error, error}

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

      defoverridable on_init: 1,
                     on_connect: 2,
                     on_disconnect: 1,
                     on_message: 2
    end
  end
end
