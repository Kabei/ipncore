defmodule Ippan.Network do
  @callback on_connect(node_id :: term(), map :: map()) :: any()
  @callback on_disconnect(state :: term()) :: any()
  @callback on_message(packet :: term(), state :: term()) :: any()
  @callback connect(node :: term(), opts :: keyword()) :: boolean()
  @callback connect_async(node :: term(), opts :: keyword()) ::
              {:ok, pid()} | true | {:error, term()}
  @callback disconnect(node_id_or_state :: binary() | term()) :: :ok
  @callback fetch(id :: term()) :: map() | nil
  @callback info(node_id :: term()) :: map() | nil
  @callback list() :: [term()]
  @callback alive?(node :: term()) :: boolean()
  @callback count() :: non_neg_integer()
  @callback cast(node_or_id :: binary | term(), message :: term) :: :ok | :disconnect
  @callback cast(node_or_id :: term(), event :: binary, data :: term) :: :ok | :disconnect
  @callback call(node_or_id :: binary | map, method :: binary) :: {:ok, term()} | {:error, term()}
  @callback call(node_or_id :: binary | map, method :: binary, message :: term) ::
              {:ok, term()} | {:error, term()}
  @callback call(
              node_or_id :: binary() | map(),
              method :: binary,
              message :: term,
              opts :: Keyword.t()
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
  # random tools
  @callback get_random_node() :: term() | nil

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
      alias IO.ANSI
      alias Ippan.P2P
      require Sqlite
      require Logger

      @behaviour Ippan.Network

      @adapter :gen_tcp
      @module __MODULE__
      @otp_app opts[:app]
      @name opts[:name]
      @table opts[:table]
      @server opts[:server] || opts[:name]
      @pubsub opts[:pubsub]
      @topic opts[:topic]
      @supervisor opts[:sup]
      @default_connect_opts opts[:conn_opts] || []

      def start_link(args) do
        GenServer.start_link(@module, args, hibernate_after: 5_000, name: __MODULE__)
      end

      @impl true
      def init(_args) do
        table =
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

        server =
          case Process.whereis(@server) do
            nil ->
              opts = Application.get_env(@otp_app, @name)
              {:ok, pid} = ThousandIsland.start_link(opts)
              Process.register(pid, @server)

              app_name =
                to_string(@otp_app)
                |> String.upcase()

              name =
                to_string(@name)
                |> String.capitalize()

              IO.puts(
                "Running #{ANSI.red() <> app_name <> ANSI.reset()} P2P #{name} with port #{ANSI.yellow() <> to_string(opts[:port]) <> ANSI.reset()}"
              )

              pid

            pid ->
              pid
          end

        {:ok, sup} = @supervisor.start_link([])

        PubSub.subscribe(@pubsub, @topic)

        state = %{sup: sup, server: server, ets: table}
        on_init(state)
        {:ok, state, {:continue, :init}}
      end

      @impl true
      def handle_continue(:init, state) do
        on_continue(state)
        {:noreply, state, :hibernate}
      end

      def on_init(_), do: :ok
      def on_continue(_), do: :ok

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
      def terminate(_reason, %{ets: ets, server: server, sup: sup}) do
        ThousandIsland.stop(server, :infinity)
        PubSub.unsubscribe(@pubsub, @topic)
        :ets.delete(ets)
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
          if socket != node.socket do
            :inet.close(socket)
          end
        end

        :ets.delete(@table, node_id)

        if Keyword.get(opts, :reconnect, false) do
          connect_async(state, opts)
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
              e ->
                Logger.error(inspect(e))
                bin = %{"_id" => id, "data" => ["error", e.message]}
                @adapter.send(state.socket, encode(bin, sharedkey))
            end

          # Receive answer
          %{"_id" => id, "data" => data} ->
            PubSub.local_broadcast(@pubsub, "call:#{id}", data)

          # Event message (no return answer)
          %{"event" => event, "data" => data} ->
            try do
              handle_message(event, data, state)
            rescue
              x ->
                Logger.error(inspect(x))
            end

          m ->
            Logger.debug(inspect(m))
            :ok
        end
      end

      @impl Network
      def connect(
            %{id: node_id, hostname: hostname, port: port, net_pubkey: net_pubkey} = node,
            opts \\ @default_connect_opts
          ) do
        unless alive?(node_id) do
          @supervisor.start_child(Map.merge(node, %{opts: opts, pid: self()}))

          receive do
            :ok -> true
            _ -> false
          end
        else
          true
        end
      end

      @impl Network
      def connect_async(%{id: node_id} = node, opts \\ @default_connect_opts) do
        unless alive?(node_id) do
          @supervisor.start_child(Map.put(node, :opts, opts))
        else
          true
        end
      end

      @impl Network
      def disconnect(%{id: node_id, socket: socket} = state) do
        :ets.delete(@table, node_id)
        @adapter.close(socket)
      end

      def disconnect(node_id) do
        case info(node_id) do
          %{socket: socket} ->
            :ets.delete(@table, node_id)
            @adapter.close(socket)

          _ ->
            :ok
        end
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
      def cast(%{sharedkey: sharedkey, socket: socket}, message) do
        @adapter.send(socket, encode(message, sharedkey))
      end

      def cast(node_id, message) do
        case info(node_id) do
          %{sharedkey: sharedkey, socket: socket} ->
            @adapter.send(socket, encode(message, sharedkey))

          _ ->
            :disconnect
        end
      end

      @impl Network
      def cast(%{sharedkey: sharedkey, socket: socket}, event, data) do
        @adapter.send(socket, encode(%{"event" => event, "data" => data}, sharedkey))
      end

      def cast(node_id, event, data) do
        case info(node_id) do
          %{sharedkey: sharedkey, socket: socket} ->
            @adapter.send(socket, encode(%{"event" => event, "data" => data}, sharedkey))

          _ ->
            :disconnect
        end
      end

      @impl Network
      def call(node, method), do: call(node, method, nil, [])

      @impl Network
      def call(node, method, message), do: call(node, method, message, [])

      @impl Network
      def call(%{sharedkey: sharedkey, socket: socket}, method, data, opts) do
        timeout = Keyword.get(opts, :timeout, 10_000)
        retry = Keyword.get(opts, :retry, 0)
        id = :rand.bytes(8)
        topic = "call:#{id}"
        message = %{"_id" => id, "method" => method, "data" => data}
        PubSub.subscribe(@pubsub, topic)
        call_retry(socket, message, sharedkey, topic, timeout, retry)
      end

      def call(node_id, method, data, opts) do
        timeout = Keyword.get(opts, :timeout, 10_000)
        retry = Keyword.get(opts, :retry, 0)
        id = :rand.bytes(8)
        topic = "call:#{id}"
        message = %{"_id" => id, "method" => method, "data" => data}

        case info(node_id) do
          nil ->
            {:error, :disconnect}

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
      def broadcast(message, role) do
        data = :ets.select(@table, [{{:_, %{role: :"$1"}}, [{:==, :"$1", role}], [:"$_"]}])

        Enum.each(data, fn {_, %{sharedkey: sharedkey, socket: socket}} ->
          @adapter.send(socket, encode(message, sharedkey))
        end)
      end

      @impl Network
      def broadcast_except(message, ids) do
        Enum.each(list(), fn {id, %{sharedkey: sharedkey, socket: socket}} ->
          if id not in ids do
            @adapter.send(socket, encode(message, sharedkey))
          end
        end)
      end

      @impl Network
      def get_random_node do
        case :ets.tab2list(@table) do
          [] -> nil
          objects -> Enum.random(objects)
        end
      end

      defoverridable on_init: 1,
                     on_continue: 1,
                     on_connect: 2,
                     on_disconnect: 1,
                     on_message: 2
    end
  end
end
