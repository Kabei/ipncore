defmodule MinerWorker do
  use GenServer
  alias Ippan.ClusterNode
  alias Ippan.Validator
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
    GenServer.call(server, {:mine, block, creator, round_id})
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
            vsn: @version
          } = block,
          creator,
          current_round
        },
        _from,
        state
      ) do
    try do
      conn = :persistent_term.get(:asset_conn)
      stmts = :persistent_term.get(:asset_stmt)
      dets = :persistent_term.get(:dets_balance)
      {node_id, node} = ClusterNode.get_random_node()

      if height != 1 + SqliteStore.one(conn, stmts, "last_block_height_created", [creator_id]) do
        raise IppanError, "Wrong block height"
      end

      # Request verify a remote blockfile
      decode_path = Block.decode_path(creator_id, height)

      # Call verify blockfile and download decode-file
      if File.exists?(decode_path) do
        :ok
      else
        # Download from Cluster node
        block_check =
          block
          |> Map.put("hostname", creator.hostname)
          |> Map.put("pubkey", creator.pubkey)

        case ClusterNode.call(node_id, "verify_block", block_check, 15_000, 2) do
          {:ok, verify_result} ->
            case verify_result do
              false ->
                raise IppanError, "Error block verify"

              true ->
                url = Block.cluster_decode_url(node.hostname, creator_id, height)
                :ok = Download.from(url, decode_path)
            end

          {:error, _} ->
            raise IppanError, "Error Node verify"
        end
      end

      Logger.debug("#{creator_id}.#{height} Txs: #{count} | #{decode_path} Mining...")

      # Read decode blockfile
      {:ok, content} = File.read(decode_path)

      %{"data" => messages, "vsn" => @version} =
        Block.decode_file!(content)

      count_rejected =
        mine_fun(@version, messages, conn, stmts, dets, creator_id, block_id)

      result =
        block
        |> Map.merge(%{round: current_round, rejected: count_rejected})
        |> Block.to_list()

      :done = SqliteStore.step(conn, stmts, "insert_block", result)

      {:reply, {:ok, count_rejected}, state}
    rescue
      error ->
        Logger.error(inspect(error))
        {:reply, :error, state}
    end
  end

  # Process the block
  defp mine_fun(0, messages, conn, stmts, dets, creator_id, block_id) do
    validator =
      SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", creator_id, Validator)

    Enum.reduce(messages, 0, fn
      [hash, type, from, args, timestamp, size], acc ->
        TxHandler.handle_regular(
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
end
