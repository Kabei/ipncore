defmodule MinerWorker do
  use GenServer
  alias Ippan.Block
  require Logger

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, hibernate_after: 10_000)
  end

  @impl true
  def init(args) do
    {:ok, args}
  end

  # Create a block file from decode block file
  @impl true
  def handle_cast(
        {:remote, from_pid,
         %{
           creator: creator_id,
           height: height,
           prev: prev_hash,
           round: round,
           timestamp: timestamp,
           ev_count: ev_count
         } = block},
        state
      ) do
    decode_path = Block.decode_path(creator_id, height)
    Logger.debug("#{creator_id}.#{height} Events: #{ev_count} | #{decode_path} Mine import")

    requests =
      if ev_count != 0 do
        # IO.inspect("import block file")
        {:ok, content} = File.read(decode_path)
        Block.decode_file!(content)
      else
        # IO.inspect("import block empty")
        []
      end

    BlockTimer.mine_fun(requests, height, round, creator_id, prev_hash, timestamp, :import)

    GenServer.cast(from_pid, {:complete, :import, block})

    {:noreply, state}
  end
end
