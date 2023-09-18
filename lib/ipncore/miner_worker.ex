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

  def mine(server, block) do
    GenServer.call(server, {:mine, block})
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
          current_round
        },
        _from,
        state
      ) do
    try do
      {node_id, _node} = ClusterNode.get_random_node()

      # Request verify a remote blockfile
      decode_path = Block.decode_path(creator_id, height)

      if File.exists?(decode_path) do
        :ok
      else
        # Download from Cluster node
        {:ok, url} = ClusterNode.call(node_id, "verify_block", block, 15_000, 2)
        :ok = Download.from(url, decode_path)
      end

      Logger.debug("#{creator_id}.#{height} Txs: #{count} | #{decode_path} Mining...")

      # read blockfile
      {:ok, content} = File.read(decode_path)

      %{"msg" => messages, "vsn" => version} =
        Block.decode_file!(content)

      conn = :persistent_term.get(:asset_conn)
      stmts = :persistent_term.get(:asset_stmt)
      dets = :persistent_term.get(:dets_balance)

      count_rejected =
        mine_fun(version, messages, conn, stmts, dets, creator_id, block_id)

      result =
        block
        |> Map.merge(%{round: current_round, rejected: count_rejected})
        |> Block.to_list()

      :done = SqliteStore.step(conn, stmts, "insert_block", result)
      :done = SqliteStore.step(conn, stmts, "delete_msg_block", [creator_id, height])

      {:reply, :ok, state}
    rescue
      error ->
        Logger.error(error.message)
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
        case TxHandler.insert_deferred(msg, block_id) do
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
