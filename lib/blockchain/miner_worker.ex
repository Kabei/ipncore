defmodule MinerWorker do
  use GenServer
  alias Ippan.Validator
  alias Ippan.EventHandler
  alias Ippan.Block
  require SqliteStore
  require Logger
  require EventHandler

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
           id: block_id,
           creator: creator_id,
           height: height,
           count: count,
           vsn: vsn
         } = block},
        state
      ) do
    decode_path = Block.decode_path(creator_id, height)
    Logger.debug("#{creator_id}.#{height} Events: #{count} | #{decode_path} Mining...")

    # {last_id, last_hash} = BlockTimer.get_last_block(creator_id)

    {:ok, content} = File.read(decode_path)

    messages =
      Block.decode_file!(content)

    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)
    dets = :persistent_term.get(:dets_balance)

    count_rejected =
      mine_fun(vsn, messages, conn, stmts, dets, creator_id, block_id)

    result =
      block
      |> Map.merge(%{round: current_round, rejected: count_rejected})
      |> Block.to_list()

    SqliteStore.step(conn, stmts, "insert_block", result)

    GenServer.cast(BlockTimer, {:complete, :block, block})

    {:noreply, state}
  end

  defp mine_fun(0, messages, conn, stmts, dets, creator_id, block_id) do
    validator =
      SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", creator_id, Validator)

    Enum.reduce(messages, 0, fn
      [hash, type, from, args, timestamp, size], acc ->
        EventHandler.handle_regular(
          conn,
          stmts,
          dets,
          validator,
          hash,
          type,
          timestamp,
          from,
          size,
          args,
          block_id
        )

        acc

      msg, acc ->
        case EventHandler.insert_deferred(msg, block_id) do
          true ->
            acc

          false ->
            acc + 1
        end
    end)
  end
end
