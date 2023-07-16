defmodule Channel do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      use GenServer
      alias Phoenix.PubSub
      require Logger
      @pubsub_server opts[:server]
      @topic opts[:topic]
      @otp_app :ipncore
      @module __MODULE__

      def start_link(opts) do
        GenServer.start_link(@module, opts, hibernate_after: 1_000, name: @module)
      end

      @impl true
      def init(args) do
        PubSub.subscribe(@pubsub_server, @topic)
        {:ok, args}
      end

      def push(server, message) do
        GenServer.cast(@module, {:push, server, message})
      end

      @impl true
      def handle_cast({:push, server, message}, state) do
        PubSub.broadcast_from(server, self(), @topic, message)
        {:noreply, state}
      end

      @impl true
      def terminate(_reason, _state) do
        PubSub.unsubscribe(@pubsub_server, @topic)
      end

      defoverridable init: 1, terminate: 2
    end
  end
end
