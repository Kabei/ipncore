defmodule BlockVerifierChannel do
  use Channel,
    server: :verifiers,
    topic: "block"

  alias Ippan.Block
  import Global, only: [miner: 0]

  def init(args) do
    PubSub.subscribe(@pubsub_server, @topic)
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch", %{hash: hash} = block, validator},
        state
      ) do
    Task.async(fn ->
      Logger.debug("block.fetch #{Base.encode16(hash)}")

      try do
        :ok = BlockTimer.verify_file!(block, validator)

        value = 1
        {:ok, signature} = Block.sign_vote(hash, value)
        vote = Map.merge(block, %{vote: value, signature: signature})

        PubSub.direct_broadcast(miner(), @pubsub_server, "block", {"valid", vote, node()})
      rescue
        _ ->
          value = -1
          {:ok, signature} = Block.sign_vote(hash, value)
          vote = Map.merge(block, %{vote: value, signature: signature})

          PubSub.direct_broadcast(miner(), @pubsub_server, "block", {"invalid", vote})
      end
    end)
    |> Task.await(:infinity)

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
