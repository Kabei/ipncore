defmodule BlockVerifierChannel do
  use Channel,
    server: :cluster,
    topic: "block"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @topic)
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch", %{hash: hash} = block, validator},
        state
      ) do
    IO.inspect(block)

    Task.async(fn ->
      Logger.debug("block.fetch #{Base.encode16(hash)}")

      try do
        :ok = BlockTimer.verify_file!(block, validator)
        Logger.debug("block.valid #{Base.encode16(hash)}")
        NodeMonitor.push(@pubsub_server, "block", {"valid", block, node()})
      rescue
        ex ->
          Logger.debug(Exception.format(:error, ex, __STACKTRACE__))
          Logger.debug("block.invalid #{Base.encode16(hash)}")
          NodeMonitor.push(@pubsub_server, "block", {"invalid", block})
      end
    end)
    |> Task.await(:infinity)

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
