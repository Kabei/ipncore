defmodule BlockVerifierChannel do
  use Channel,
    server: :verifiers,
    channel: "block"

  @otp_app :ipncore
  @send_to :miner
  @file_extension "erl"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    PubSub.subscribe(@pubsub_server, "#{@channel}:#{node()}")
    Logger.debug("sub: #{@channel}:#{node()}")
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch", %{hostname: hostname, origin: origin},
         %{
           hash: hash,
           creator: vid,
           height: height
         } = block},
        state
      ) do
    Logger.debug("block.fetch #{Base.encode16(hash)}")
    decode_dir = Application.get_env(@otp_app, :decode_dir)
    filename = "#{vid}.#{height}.#{@file_extension}"
    block_path = Path.join(decode_dir, filename)

    try do
      unless File.exists?(block_path) do
        url = "https://#{hostname}/v1/download/block/#{vid}/#{height}"
        {:ok, _path} = Curl.download_block(url, block_path)
      end

      validator = ValidatorStore.lookup([vid])
      BlockTimer.verify_block!(block, validator)

      PubSub.broadcast(
        @send_to,
        "block",
        {"valid:#{hash}", :ok, block, origin}
      )
    rescue
      _ -> PubSub.broadcast(@send_to, "block:#{hash}", {"valid", :error, block, origin})
    end

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
