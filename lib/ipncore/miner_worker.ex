defmodule MinerWorker do
  use GenServer
  alias Ippan.{Account, Block, TxHandler, Validator}
  alias Ippan.ClusterNodes
  require Ippan.{Block, Validator, TxHandler}
  require Sqlite
  require Logger

  @app Mix.Project.config()[:app]
  @version Application.compile_env(@app, :version)

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, hibernate_after: 10_000)
  end

  @impl true
  def init(args) do
    {:ok, args}
  end

  def mine(server, block, creator, round_id, verify_block) do
    GenServer.call(server, {:mine, block, creator, round_id, verify_block}, :infinity)
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
          current_round_id,
          verify_block
        },
        _from,
        state
      ) do
    db_ref = :persistent_term.get(:main_conn)

    try do
      IO.puts("Here 0")

      %{height: block_height, hash: prev_hash} =
        Block.last_created(creator_id)

      IO.puts("height #{height} sql-height #{block_height}")

      IO.puts(block_height)

      if height != 1 + block_height do
        raise IppanError, "Wrong block height #{height} | #{block_height}"
      end

      # Request verify a remote blockfile
      decode_path = Block.decode_path(creator_id, height)

      IO.puts("Here 2")
      # Call verify blockfile and download decode-file
      unless File.exists?(decode_path) do
        # Download from Cluster node
        block_check =
          block
          |> Map.put("hostname", creator.hostname)
          |> Map.put("round", current_round_id)

        case verify_block do
          true ->
            case random_node_verify(block_check) do
              {:ok, node} ->
                url = Block.cluster_decode_url(node.hostname, creator_id, height)
                :ok = Download.from(url, decode_path)

              :error ->
                raise IppanError, "Error block verify"
            end

          false ->
            # download remote decode-file
            url = Block.decode_url(creator.hostname, creator_id, height)
            :ok = DownloadTask.start(url, decode_path)
        end
      end

      Logger.debug("#{creator_id}.#{height} Txs: #{count} | #{decode_path} Mining...")

      IO.puts("Here 3")
      # Read decode blockfile
      {:ok, content} = File.read(decode_path)

      IO.puts("Here 4")

      %{"data" => txs, "vsn" => version_file} =
        Block.decode_file!(content)

      if version != version_file or version != @version,
        do: raise(IppanError, "Block file version failed")

      IO.puts("Here 5")

      count_rejected =
        run_miner(current_round_id, block_id, creator, txs)

      IO.puts("Here 6")

      result =
        block
        |> Map.merge(%{
          prev: prev_hash,
          round: current_round_id,
          rejected: count_rejected,
          status: 0
        })

      IO.puts("Here 7")
      :done = Block.insert(Block.to_list(result))

      {:reply, {:ok, result}, state}
    rescue
      error ->
        Logger.error(Exception.format(:error, error, __STACKTRACE__))

        # delete player
        Validator.delete(creator_id)
        ClusterNodes.broadcast(%{"event" => "validator.leave", "data" => creator_id})
        b = Block.cancel(block, current_round_id, count, 1)
        :done = Block.insert(Block.to_list(b))
        {:reply, {:error, b}, state}
    end
  end

  # Process the block
  defp run_miner(round_id, block_id, validator, transactions) do
    nonce_dets = DetsPlux.get(:nonce)
    nonce_tx = DetsPlux.tx(nonce_dets, :nonce)
    dtx = :ets.whereis(:dtx)
    dtmp = :ets.new(:tmp, [:set])
    # 1. tx counter
    # 2. errors counter
    cref = :counters.new(2, [])

    Enum.each(transactions, fn
      ["error", _hash, _type, _from, _nonce, _args, _sig, _size] ->
        :counters.add(cref, 2, 1)

      [hash, type, from, nonce, args, _sig, size] ->
        Account.gte_nonce(nonce_dets, nonce_tx, from, nonce)

        case TxHandler.regular() do
          {:error, _} ->
            :counters.add(cref, 2, 1)

          :error ->
            :counters.add(cref, 2, 1)

          _ ->
            nil
        end

        :counters.add(cref, 1, 1)

      [hash, type, arg_key, from, nonce, args, _sig, size] ->
        ix = :counters.get(cref, 1)

        Account.gte_nonce(nonce_dets, nonce_tx, from, nonce)

        case TxHandler.insert_deferred(dtx, dtmp) do
          true ->
            nil

          false ->
            :counters.add(cref, 2, 1)
        end

        :counters.add(cref, 1, 1)
    end)

    :ets.delete(dtmp)
    :counters.get(cref, 2)
  end

  defp random_node_verify(block) do
    IO.inspect("random_node_verify")

    case ClusterNodes.get_random_node() do
      nil ->
        IO.inspect("random_node_verify: nil")
        :timer.sleep(500)
        random_node_verify(block)

      {node_id, node} ->
        case ClusterNodes.call(node_id, "verify_block", block,
               timeout: 10_000,
               retry: 2
             ) do
          {:ok, 1} ->
            IO.inspect("random_node_verify Call 1")
            {:ok, node}

          {:ok, 0} ->
            IO.inspect("random_node_verify Call 0")
            :error

          {:ok, 2} ->
            IO.inspect("random_node_verify Call 2")
            :timer.sleep(500)
            random_node_verify(block)

          {:error, _} ->
            IO.inspect("random_node_verify Call ERROR")
            :timer.sleep(500)
            random_node_verify(block)
        end
    end
  end
end
