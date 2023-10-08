defmodule MinerWorker do
  use GenServer
  alias Ippan.ClusterNodes
  alias Ippan.TxHandler
  alias Ippan.Block
  require SqliteStore
  require TxHandler
  require Logger

  @version Application.compile_env(:ipncore, :version)

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, hibernate_after: 10_000)
  end

  @impl true
  def init(args) do
    {:ok, args}
  end

  def mine(server, block, creator, round_id) do
    GenServer.call(server, {:mine, block, creator, round_id}, :infinity)
  end

  # Create a block file from decode block file (foreign block)
  @impl true
  def handle_call(
        {
          :mine,
          %{
            id: block_id,
            creator: creator_id,
            height: height,
            count: count,
            vsn: version
          } = block,
          creator,
          current_round_id
        },
        _from,
        state
      ) do
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)

    try do
      IO.puts("Here 0")
      balances = DetsPlux.whereis(:balance)

      [block_height, prev_hash] =
        SqliteStore.fetch(conn, stmts, "last_block_created", [creator_id], [-1, nil])

      IO.puts("height #{height} sql-height #{block_height}")

      IO.puts(block_height)

      if height != 1 + block_height do
        raise IppanError, "Wrong block height"
      end

      # Request verify a remote blockfile
      decode_path = Block.decode_path(creator_id, height)

      IO.puts("Here 2")
      # Call verify blockfile and download decode-file
      if File.exists?(decode_path) do
        :ok
      else
        # Download from Cluster node
        block_check =
          block
          |> Map.put("hostname", creator.hostname)
          |> Map.put("pubkey", creator.pubkey)

        {node_id, node} = random_node()

        case ClusterNodes.call(node_id, "verify_block", block_check, 10_000, 2) do
          {:ok, true} ->
            url = Block.cluster_decode_url(node.hostname, creator_id, height)
            :ok = Download.from(url, decode_path)

          {:ok, false} ->
            raise IppanError, "Error block verify"

          {:error, _} ->
            raise IppanError, "Error Node verify"
        end
      end

      Logger.debug("#{creator_id}.#{height} Txs: #{count} | #{decode_path} Mining...")

      IO.puts("Here 3")
      # Read decode blockfile
      {:ok, content} = File.read(decode_path)

      IO.puts("Here 4")

      %{"data" => messages, "vsn" => version_file} =
        Block.decode_file!(content)

      if version != version_file, do: raise(IppanError, "Block file version failed")

      IO.puts("Here 5")

      count_rejected =
        mine_fun(version, messages, conn, stmts, balances, creator, block_id)

      IO.puts("Here 6")

      result =
        block
        |> Map.merge(%{prev: prev_hash, round: current_round_id, rejected: count_rejected})

      IO.puts("Here 7")
      :done = SqliteStore.step(conn, stmts, "insert_block", Block.to_list(result))

      {:reply, {:ok, result}, state}
    rescue
      error ->
        # delete player
        SqliteStore.step(conn, stmts, "delete_validator", [creator_id])
        ClusterNodes.broadcast(%{"event" => "validator.delete", "data" => creator_id})

        Logger.error(inspect(error))
        {:reply, :error, state}
    end
  end

  # Process the block
  defp mine_fun(@version, messages, conn, stmts, balances, validator, block_id) do
    creator_id = validator.id

    Enum.reduce(messages, 0, fn
      [hash, type, from, args, timestamp, size], acc ->
        case TxHandler.handle_regular(
               conn,
               stmts,
               balances,
               validator,
               hash,
               type,
               from,
               args,
               size,
               timestamp,
               block_id
             ) do
          :error -> acc + 1
          _ -> acc
        end

      msg, acc ->
        case TxHandler.insert_deferred(msg, creator_id, block_id) do
          true ->
            acc

          false ->
            acc + 1
        end
    end)
  end

  defp mine_fun(version, _messages, _conn, _stmts, _dets, _creator_id, _block_id) do
    raise IppanError, "Error block version #{inspect(version)}"
  end

  defp random_node do
    case ClusterNodes.get_random_node() do
      nil ->
        :timer.sleep(1_000)
        random_node()

      result ->
        result
    end
  end
end
