defmodule MinerWorker do
  use GenServer
  alias Ippan.{Block, TxHandler, Validator, Wallet}
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

      [block_height, prev_hash] =
        Block.last_created(creator_id, [-1, nil])

      IO.puts("height #{height} sql-height #{block_height}")

      IO.puts(block_height)

      if height != 1 + block_height do
        raise IppanError, "Wrong block height #{height} | #{block_height}"
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

        {node_id, node} = random_node()

        case verify_block do
          true ->
            case ClusterNodes.call(node_id, "verify_block", block_check,
                   timeout: 10_000,
                   retry: 2
                 ) do
              {:ok, true} ->
                url = Block.cluster_decode_url(node.hostname, creator_id, height)
                :ok = Download.from(url, decode_path)

              {:ok, false} ->
                raise IppanError, "Error block verify"

              {:error, _} ->
                raise IppanError, "Error Node verify"
            end

          false ->
            url = Block.url(creator.hostname, creator_id, height)
            :ok = Download.from(url, decode_path)
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
        b = Block.cancel(block, current_round_id, 1)
        :done = Block.insert(Block.to_list(b))
        {:reply, :error, state}
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
      [hash, type, from, nonce, args, _sig, size] ->
        case Wallet.gte_nonce(nonce_dets, nonce_tx, from, nonce) do
          :error ->
            :counters.add(cref, 2, 1)

          _true ->
            case TxHandler.regular() do
              {:error, _} ->
                :counters.add(cref, 2, 1)

              :error ->
                :counters.add(cref, 2, 1)

              _ ->
                nil
            end
        end

        :counters.add(cref, 1, 1)

      [hash, type, arg_key, from, nonce, args, _sig, size] ->
        case Wallet.gte_nonce(nonce_dets, nonce_tx, from, nonce) do
          :error ->
            :counters.add(cref, 2, 1)

          _true ->
            ix = :counters.get(cref, 1)

            case TxHandler.insert_deferred(dtx, dtmp) do
              true ->
                nil

              false ->
                :counters.add(cref, 2, 1)
            end
        end

        :counters.add(cref, 1, 1)
    end)

    :ets.delete(dtmp)
    :counters.get(cref, 2)
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
