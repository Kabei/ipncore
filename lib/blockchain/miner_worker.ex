defmodule MinerWorker do
  use GenServer
  alias Ippan.Validator
  alias Ippan.EventHandler
  alias Ippan.Block
  require SqliteStore
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
        {:mine, _from_pid, current_round,
         %{
           creator: creator_id,
           height: height,
           count: count
         } = block},
        state
      ) do
    decode_path = Block.decode_path(creator_id, height)
    Logger.debug("#{creator_id}.#{height} Events: #{count} | #{decode_path} Mining...")

    # {last_id, last_hash} = BlockTimer.get_last_block(creator_id)

    messages =
      if count != 0 do
        # IO.inspect("block file")
        {:ok, content} = File.read(decode_path)
        Block.decode_file!(content)
      else
        # IO.inspect("block zero")
        []
      end

    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)
    dets = :persistent_term.get(:dets_balance)

    mine_fun(messages, conn, stmts, dets, creator_id, current_round)

    result = block |> Map.put(:round, current_round) |> Block.to_list()

    SqliteStore.step(conn, stmts, "insert_block", result)

    GenServer.cast(BlockTimer, {:complete, :block, block})

    {:noreply, state}
  end

  defp mine_fun([], _, _, _, _, _), do: :ok

  defp mine_fun(messages, conn, stmts, dets, creator_id, round) do
    validator =
      SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", creator_id, Validator)

    for [hash, timestamp, type, from, args, size] <- messages do
      EventHandler.handle!(
        conn,
        stmts,
        dets,
        hash,
        type,
        timestamp,
        from,
        validator,
        size,
        args,
        round
      )
    end
  end
end
