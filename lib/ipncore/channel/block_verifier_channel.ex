defmodule BlockVerifierChannel do
  use Channel,
    server: :verifiers,
    channel: "block"

  @otp_app :ipncore
  @send_to :miner
  @file_extension "erl"

  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    PubSub.subscribe(@pubsub_server, "#{@channel}#{node()}")
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch",
         %{
           hash: hash,
           creator: vid,
           hostname: hostname,
           height: height
         } = block},
        state
      ) do
    decode_dir = Application.get_env(@otp_app, :decode_dir)
    filename = "#{vid}.#{height}.#{@file_extension}"
    block_path = Path.join(decode_dir, filename)

    url = "https://#{hostname}/v1/download/block/#{filename}"
    Download.from(url, path: block_path)
    local_hostname = Application.get_env(:ipncore, :hostname)

    try do
      validator = ValidatorStore.lookup([vid])
      BlockTimer.verify_block!(block, validator)

      PubSub.broadcast(
        @send_to,
        "block",
        {"valid:#{hash}", :ok, block, local_hostname}
      )
    rescue
      _ -> PubSub.broadcast(@send_to, "block:#{hash}", {"valid", :error, block, local_hostname})
    end

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end
