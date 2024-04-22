defmodule Ipncore.MinerWorker do
  alias Ippan.{DetsSup, ClusterNodes}
  alias Ippan.Block
  alias Ippan.TxHandler
  alias Ippan.Funcs
  alias Ippan.Validator
  require Sqlite
  require Ippan.Validator

  @download_cluster_options [retry: :infinity, time_to_retry: 100]
  # @download_options [retry: 5, time_to_retry: 250]

  def build(round_id, blocks, verify) do
    cref = :counters.new(2, [])
    txd = :ets.new(:txd, [:duplicate_bag])

    refs = %{
      db_ref: :persistent_term.get(:main_conn),
      cref: cref,
      dets: DetsSup.dets(),
      txs: DetsSup.txs(),
      txd: txd,
      verify: verify,
      error: :ets.new(:error, [:duplicate_bag, :protected])
    }

    do_mine(round_id, refs, blocks)
    do_run_deferred(txd)

    %{
      rejected: :counters.get(cref, 2)
    }
  end

  defp do_mine(_round_id, _refs, []), do: :ok

  defp do_mine(round_id, refs, [block | blocks]) do
    mine(round_id, block, refs)
    do_mine(round_id, refs, blocks)
  end

  @partitions TxSupervisor.partitions()
  defp mine(
         round_id,
         block = %{
           creator: creator_id,
           height: height
         },
         %{db_ref: db_ref, cref: cref, error: _ets_error, txd: txd, verify: verify} = _refs
       ) do
    decode_path = Block.decode_path(creator_id, height)
    creator = Validator.get(creator_id)

    {ptxs, errors} = get_transactions(creator, round_id, decode_path, block, verify)
    :counters.add(cref, 2, errors)

    pids = TxWorker.all()

    Enum.each(ptxs, fn
      {"D", txs} ->
        :ets.insert(txd, txs)

      {_number, txs} ->
        Enum.each(
          txs,
          fn tx = {_hash, type_id, from, _nonce, args, _size, _signature} ->
            type = %{flag: flag} = Funcs.lookup(type_id)
            pnum = TxHandler.get_part(flag, from, args, @partitions)
            pid = Map.get(pids, pnum)

            # send transaction to process
            :gen_server.cast(pid, {:run, tx, type})
          end
        )
    end)
  end

  defp do_run_deferred(tid) do
    pids = TxWorker.all()
    do_run_deferred(:ets.first(tid), tid, pids)
  end

  defp do_run_deferred(:"$end_of_table", _tid, _pids), do: :ok

  defp do_run_deferred(key, tid, pids) do
    [tx = {_hash, type_id, from, _nonce, args, _size, _signature}] = :ets.lookup(tid, key)
    type = %{flag: flag} = Funcs.lookup(type_id)
    pnum = TxHandler.get_part(flag, from, args, @partitions)
    pid = Map.get(pids, pnum)
    :gen_server.cast(pid, {:run, tx, type})

    do_run_deferred(:ets.next(tid, key), tid, pids)
  end

  defp get_transactions(creator, round_id, output_path, block, verify) do
    # Get or/and Verify blockfile
    cond do
      File.exists?(output_path) ->
        # do not download
        :ok

      verify == false ->
        # download block from remote node
        url = Block.decode_url(creator.hostname, creator.id, block.height)
        :ok = DownloadTask.start(url, output_path, @download_cluster_options)

      true ->
        block =
          block
          |> Map.put("hostname", creator.hostname)
          |> Map.put("round", round_id)

        # verify block
        case random_node_verify(block) do
          {:ok, node} ->
            # download block from cluster
            url = Block.cluster_decode_url(node.hostname, creator.id, block.height)
            :ok = DownloadTask.start(url, output_path, @download_cluster_options)

          :error ->
            {:error, "Error block verify"}
        end
    end

    {:ok, content} = File.read(output_path)
    {:ok, %{"txs" => transactions, "errors" => errors}, _} = CBOR.decode(content)
    {transactions, errors}
  end

  defp random_node_verify(block) do
    IO.inspect("random_node_verify")

    case ClusterNodes.get_random_node() do
      nil ->
        IO.inspect("random_node_verify: nil")
        :timer.sleep(200)
        random_node_verify(block)

      {node_id, node} ->
        case ClusterNodes.call(node_id, "verify_block", block,
               timeout: 10_000,
               retry: 1
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

# defmodule MinerWorker do
#   use GenServer
#   alias Ippan.{Account, Block, TxHandler}
#   alias Ippan.ClusterNodes
#   require Ippan.{Block, Validator, TxHandler}
#   require Sqlite
#   require Logger

#   @app Mix.Project.config()[:app]
#   @version Application.compile_env(@app, :version)

#   def start_link(_) do
#     GenServer.start_link(__MODULE__, nil, hibernate_after: 10_000)
#   end

#   @impl true
#   def init(args) do
#     {:ok, args}
#   end

#   def mine(server, block, creator, round_id, verify_block) do
#     GenServer.call(server, {:mine, block, creator, round_id, verify_block}, :infinity)
#   end

#   @download_cluster_options [retry: :infinity, time_to_retry: 100]
#   @download_options [retry: 5, time_to_retry: 250]

#   # Create a block file from decode block file (foreign block)
#   @impl true
#   def handle_call(
#         {
#           :mine,
#           %{
#             id: block_id,
#             creator: creator_id,
#             height: height,
#             count: count,
#             vsn: version
#           } = block,
#           creator,
#           current_round_id,
#           verify_block
#         },
#         _from,
#         state
#       ) do
#     db_ref = :persistent_term.get(:main_conn)

#     try do
#       IO.puts("Here 0")

#       %{height: block_height, hash: prev_hash} =
#         Block.last_created(creator_id)

#       IO.puts("height #{height} sql-height #{block_height}")

#       IO.puts(block_height)

#       if height != 1 + block_height do
#         raise IppanError, "Wrong block height #{height} | #{block_height}"
#       end

#       # Request verify a remote blockfile
#       decode_path = Block.decode_path(creator_id, height)

#       IO.puts("Here 2")
#       # Call verify blockfile and download decode-file
#       unless File.exists?(decode_path) do
#         # Download from Cluster node
#         block_check =
#           block
#           |> Map.put("hostname", creator.hostname)
#           |> Map.put("round", current_round_id)

#         case verify_block do
#           true ->
#             case random_node_verify(block_check) do
#               {:ok, node} ->
#                 url = Block.cluster_decode_url(node.hostname, creator_id, height)
#                 :ok = DownloadTask.start(url, decode_path, @download_cluster_options)

#               :error ->
#                 raise IppanError, "Error block verify"
#             end

#           false ->
#             # download remote decode-file
#             url = Block.decode_url(creator.hostname, creator_id, height)
#             :ok = DownloadTask.start(url, decode_path, @download_options)
#         end
#       end

#       Logger.debug("#{creator_id}.#{height} Txs: #{count} | #{decode_path} Mining...")

#       IO.puts("Here 3")
#       # Read decode blockfile
#       {:ok, content} = File.read(decode_path)

#       IO.puts("Here 4")

#       %{"data" => txs, "vsn" => version_file} =
#         Block.decode_file!(content)

#       if version != version_file or version != @version,
#         do: raise(IppanError, "Block file version failed")

#       IO.puts("Here 5")

#       count_rejected =
#         run_miner(current_round_id, block_id, creator, txs)

#       IO.puts("Here 6")

#       result =
#         block
#         |> Map.merge(%{
#           prev: prev_hash,
#           round: current_round_id,
#           rejected: count_rejected,
#           status: 0
#         })

#       IO.puts("Here 7")
#       :done = Block.insert(Block.to_list(result))

#       {:reply, {:ok, result}, state}
#     rescue
#       error ->
#         Logger.error(Exception.format(:error, error, __STACKTRACE__))

#         # delete player
#         # Validator.delete(creator_id)
#         # ClusterNodes.broadcast(%{"event" => "validator.leave", "data" => creator_id})
#         b = Block.cancel(block, current_round_id, count, 1)
#         :done = Block.insert(Block.to_list(b))
#         {:reply, {:error, b}, state}
#     end
#   end

#   # Process the block
#   defp run_miner(round_id, block_id, validator, transactions) do
#     nonce_dets = DetsPlux.get(:nonce)
#     nonce_tx = DetsPlux.tx(nonce_dets, :nonce)
#     dtx = :ets.whereis(:dtx)
#     dtmp = :ets.new(:tmp, [:set])
#     # 1. tx counter
#     # 2. errors counter
#     cref = :counters.new(2, [])

#     Enum.each(transactions, fn
#       ["err", _hash, _type, from, nonce, _args, _sig, _size] ->
#         Account.gte_nonce(nonce_dets, nonce_tx, from, nonce)
#         :counters.add(cref, 2, 1)

#       [hash, type, from, nonce, args, _sig, size] ->
#         Account.gte_nonce(nonce_dets, nonce_tx, from, nonce)

#         case TxHandler.regular() do
#           {:error, _} ->
#             :counters.add(cref, 2, 1)

#           :error ->
#             :counters.add(cref, 2, 1)

#           _ ->
#             nil
#         end

#         :counters.add(cref, 1, 1)

#       [hash, type, arg_key, from, nonce, args, _sig, size] ->
#         ix = :counters.get(cref, 1)
#         Account.gte_nonce(nonce_dets, nonce_tx, from, nonce)

#         case TxHandler.insert_deferred(dtx, dtmp) do
#           true ->
#             nil

#           false ->
#             :counters.add(cref, 2, 1)
#         end

#         :counters.add(cref, 1, 1)
#     end)

#     :ets.delete(dtmp)
#     :counters.get(cref, 2)
#   end

#   defp random_node_verify(block) do
#     IO.inspect("random_node_verify")

#     case ClusterNodes.get_random_node() do
#       nil ->
#         IO.inspect("random_node_verify: nil")
#         :timer.sleep(250)
#         random_node_verify(block)

#       {node_id, node} ->
#         case ClusterNodes.call(node_id, "verify_block", block,
#                timeout: 10_000,
#                retry: 2
#              ) do
#           {:ok, 1} ->
#             IO.inspect("random_node_verify Call 1")
#             {:ok, node}

#           {:ok, 0} ->
#             IO.inspect("random_node_verify Call 0")
#             :error

#           {:ok, 2} ->
#             IO.inspect("random_node_verify Call 2")
#             :timer.sleep(500)
#             random_node_verify(block)

#           {:error, _} ->
#             IO.inspect("random_node_verify Call ERROR")
#             :timer.sleep(500)
#             random_node_verify(block)
#         end
#     end
#   end
# end
