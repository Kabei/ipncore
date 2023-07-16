defmodule BlockVerifierChannel do
  use Channel,
    server: :verifiers,
    channel: "block"

  alias Ippan.Block
  import Global, only: [miner: 0]

  @otp_app :ipncore
  @file_extension "erl"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    Logger.debug("sub: #{@channel}:#{node()}")
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch",
         %{
           hash: hash,
           creator: vid,
           height: height
         } = block, %{hostname: hostname} = validator},
        state
      ) do
    Task.async(fn ->
      hash16 = Base.encode16(hash)
      Logger.debug("block.fetch #{hash16}")
      decode_dir = Application.get_env(@otp_app, :decode_dir)
      filename = "#{vid}.#{height}.#{@file_extension}"
      block_path = Path.join(decode_dir, filename)

      try do
        unless File.exists?(block_path) do
          url = "https://#{hostname}/v1/download/block/#{vid}/#{height}"
          {:ok, _path} = Curl.download_block(url, block_path)
        end

        BlockTimer.verify!(block, validator)

        value = 1
        signature = Block.sign_vote(hash, value)
        vote = Map.merge(block, %{vote: value, signature: signature})

        PubSub.direct_broadcast(miner(), @pubsub_server, "block", {"valid", vote, node()})
      rescue
        _ ->
          value = -1
          signature = Block.sign_vote(hash, value)
          vote = Map.merge(block, %{vote: value, signature: signature})

          PubSub.direct_broadcast(miner(), @pubsub_server, "block", {"valid", vote, node()})
      end

      # File.rm(block_path)
    end)
    |> Task.await(:infinity)

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
