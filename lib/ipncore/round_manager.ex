defmodule RoundManager do
  use GenServer, restart: :transient
  require Logger
  alias Ippan.ClusterNode
  alias Ippan.Block
  alias Ippan.Round
  alias Ippan.{NetworkNode, Validator, MsgBlock, MsgRound}
  alias Phoenix.PubSub
  require SqliteStore

  @miner_pool :miner_pool
  @pubsub :cluster
  @timeout 20_000
  @block_interval Application.compile_env(:ipncore, :block_interval)
  @max_peers_conn Application.compile_env(:ipncore, :max_peers_conn)

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    vid = :persistent_term.get(:vid)
    main = {conn = :persistent_term.get(:asset_conn), stmts = :persistent_term.get(:asset_stmt)}
    net = {:persistent_term.get(:net_conn), :persistent_term.get(:net_stmt)}

    ets_players =
      :ets.new(:players, [:ordered_set, :public, read_concurrency: true, write_concurrency: true])

    # ets_votes =
    #   :ets.new(:votes, [:set, :public, read_concurrency: true, write_concurrency: true])

    [round_id, round_hash] =
      SqliteStore.fetch(conn, stmts, "last_round", [], [0, nil])

    [block_id, block_hash] =
      SqliteStore.fetch(conn, stmts, "last_block_created", [vid], [0, nil])

    # Subscribe
    PubSub.subscribe(@pubsub, "validator")
    PubSub.subscribe(@pubsub, "env")

    # Get all players
    players = SqliteStore.all(conn, stmts, "get_players")
    total_players = length(players)

    miner_pool_pid = start_miner_pool()

    # Fill data to table of players
    for v <- players do
      :ets.insert(ets_players, Validator.list_to_tuple(v))
    end

    {:ok, bRef} = :timer.send_after(@block_interval, :mine)

    {:ok,
     %{
       block_id: block_id,
       block_hash: block_hash,
       candidate: nil,
       main: main,
       miner_pool: miner_pool_pid,
       net: net,
       players: ets_players,
       #  ets_votes: ets_votes,
       turn: false,
       position: nil,
       rcid: nil,
       rc_node: nil,
       round_id: round_id,
       round_hash: round_hash,
       total: total_players,
       tRef: nil,
       bRef: bRef,
       vid: vid
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(:next, %{tRef: tRef} = state) do
    new_state = get_state_after_check(state)

    :timer.cancel(tRef)

    if new_state.turn do
      IO.puts("Its my turn")

      pid = self()

      spawn_link(fn ->
        build_round(pid, new_state)
      end)

      {:noreply, %{new_state | candidate: nil}, :hibernate}
    else
      {:ok, tRef} = :timer.send_after(@timeout, :timeout)
      {:noreply, %{new_state | tRef: tRef}, :hibernate}
    end
  end

  @impl true
  def handle_info(:timeout, %{round_id: round_id} = state) do
    IO.puts("Round ##{round_id} Timeout")
    {:noreply, state}
  end

  # Check
  def handle_info(:mine, %{bRef: bRef, candidate: nil, vid: vid, block_id: block_id} = state) do
    :timer.cancel(bRef)
    result = BlockHandler.generate_files(vid, block_id)
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:noreply, %{state | bRef: bRef, candidate: result}, :hibernate}
  end

  def handle_info(:mine, state) do
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:noreply, %{state | bRef: bRef}, :hibernate}
  end

  def handle_info(
        {
          "msg_round",
          msg_round = %{
            "id" => id,
            "creator" => creator,
            "messages" => _messages,
            "hash" => hash,
            "signature" => signature
          },
          node_id
        },
        %{
          net: {net_conn, net_stmts},
          players: ets_players,
          # ets_votes: ets_votes,
          # min: min_votes,
          # delegates: delegates,
          rcid: rcid,
          round_id: round_id
        } =
          state
      )
      when id > round_id do
    #  count <-
    #    :ets.update_counter(ets_votes, id, {3, 1}, {id, msg_round, 1})
    with true <- creator == rcid,
         [{_, player}] <- :ets.lookup(ets_players, creator),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey) do
      NetworkNode.broadcast_except(%{"event" => "msg_round", "data" => msg_round}, [node_id])

      # if count == min_votes do
      case SqliteStore.step(net_conn, net_stmts, "insert_msg_round", MsgRound.encode(msg_round)) do
        :done ->
          send(self(), {:sync, msg_round})
          {:noreply, state}

        :busy ->
          Logger.error("Database Busy")
          {:noreply, state}

        _ ->
          {:noreply, state}
      end

      # else
      #   {:noreply, state}
      # end
    end
  end

  def handle_info(
        %{
          "event" => "msg_block",
          "data" =>
            block = %{
              "creator" => creator,
              "height" => height,
              "hash" => hash,
              "signature" => signature
            }
        },
        %{main: {conn, stmts}, net: {net_conn, net_stmts}, players: ets_players} = state
      ) do
    with [{_, player}] <- :ets.lookup(ets_players, creator),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         false <- SqliteStore.exists?(conn, stmts, "exists_block", [creator, height]),
         :done <-
           SqliteStore.step(net_conn, net_stmts, "insert_msg_block", MsgBlock.encode(block)) do
      {:noreply, state}
    else
      _ ->
        {:noreply, state}
    end
  end

  def handle_info(
        %{"event" => "validator.new", "data" => validator},
        %{players: ets_players} = state
      ) do
    :ets.insert(ets_players, Validator.to_tuple(validator))
    total_players = get_total_players(ets_players)
    new_state = get_state_after_check(%{state | total: total_players + 1})

    {:noreply, new_state}
  end

  def handle_info(
        %{"event" => "validator.delete", "data" => validator_id},
        %{players: ets_players} = state
      ) do
    :ets.delete(ets_players, validator_id)
    total_players = get_total_players(ets_players)
    new_state = get_state_after_check(%{state | total: total_players + 1})

    {:noreply, new_state}
  end

  @impl true
  def handle_call(
        :candidate,
        _from,
        %{bRef: bRef, candidate: nil, vid: vid, block_id: block_id} = state
      ) do
    :timer.cancel(bRef)
    result = BlockHandler.generate_files(vid, block_id)
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:reply, result, %{state | bRef: bRef, candidate: result}, :hibernate}
  end

  def handle_call(:candidate, state) do
    {:reply, nil, state, :hibernate}
  end

  @impl true
  # Process round
  def handle_cast({:complete, round}, state) do
    Logger.debug("Round completed: #{round.id} | #{Base.encode16(round.hash)}")
    # run deferred tx
    # run jackpot and events
    # save all round
    {:noreply, %{state | round_id: round.id, round_hash: round.hash}}
    # {:noreply, %{state | round_id: round.id, round_hash: round.hash}, {:continue, :next}}
  end

  @impl true
  def terminate(_reason, %{players: ets_players, miner_pool: miner_pool_pid}) do
    :ets.delete(ets_players)
    :poolboy.stop(miner_pool_pid)
    PubSub.unsubscribe(@pubsub, "validator")
  end

  def get_candidate(pid) do
    GenServer.call(pid, :candidate, :infinity)
  end

  # defp get_list_delegates(round, total_players, total_delegates) do
  #   p = total_players - 1
  #   start_pos = div(rem(round, total_players), total_delegates) * total_delegates
  #   end_pos = start_pos + total_delegates - 1
  #   rest = end_pos - p

  #   cond do
  #     rest > 0 ->
  #       Enum.to_list(start_pos..p) ++ Enum.to_list(0..(rest - 1))

  #     true ->
  #       Enum.to_list(start_pos..end_pos)
  #   end
  # end

  # defp get_vid_delegates(ets_players, list_id) do
  #   for i <- list_id do
  #     [{key, _}] = :ets.slot(ets_players, i)
  #     key
  #   end
  # end

  defp start_miner_pool do
    case Process.whereis(@miner_pool) do
      nil ->
        {:ok, pid} = :poolboy.start_link(worker_module: MinerWorker, size: 5, max_overflow: 2)
        Process.register(pid, @miner_pool)
        pid

      pid ->
        pid
    end
  end

  defp build_round(pid, %{
         round_id: round_id,
         round_hash: round_hash,
         vid: creator,
         main: {conn, stmts},
         miner_pool: pool,
         candidate: candidate
       }) do
    candidate =
      if candidate do
        candidate
      else
        get_candidate(pid)
      end

    limit = EnvStore.round_blocks()

    blocks =
      ([candidate] ++
         SqliteStore.fetch_all(conn, stmts, "msg_blocks", limit))
      |> Enum.filter(&(not is_nil(&1)))
      |> Enum.map(fn x ->
        MsgBlock.decode(x)
      end)

    {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)

    hash = Round.compute_hash(round_id, round_hash, creator, hashes)
    {:ok, signature} = Cafezinho.Impl.sign(hash, :persistent_term.get(:privkey))
    reward = Round.reward(tx_count, size)
    block_count = length(hashes)

    round = %Round{
      id: round_id,
      creator: creator,
      hash: hash,
      prev: round_hash,
      signature: signature,
      coinbase: reward,
      count: block_count,
      tx_count: tx_count,
      size: size,
      blocks: hashes,
      extra: nil
    }

    :done = SqliteStore.step(conn, stmts, "insert_round", Round.to_list(round))

    # Tasks to create blocks
    Enum.map(blocks, fn block ->
      Task.async(fn ->
        :poolboy.transaction(
          pool,
          fn pid ->
            MinerWorker.mine(pid, block)
          end,
          :infinity
        )
      end)
    end)
    |> Enum.each(&Task.await(&1, :infinity))

    GenServer.cast(pid, {:complete, round})

    NetworkNode.broadcast(%{"event" => "msg_round", "data" => round})
    ClusterNode.broadcast(%{"event" => "round.new", "data" => round})
  end

  # Check turn of the round and connect to round creator, check another connections
  defp get_state_after_check(
         %{players: ets_players, round_id: round_id, total: total_players, vid: vid} = state
       ) do
    position = get_position(round_id, total_players)
    {rcid, rc_node} = get_round_creator(ets_players, position)
    turn = rcid == vid

    IO.inspect("RCID: " <> inspect(rcid))
    IO.inspect("Position: " <> inspect(position))
    IO.inspect("Turn: " <> inspect(turn))

    new_state = %{state | position: position, rcid: rcid, rc_node: rc_node, turn: turn}

    connect_to_peers(ets_players, total_players)
    sync_to_round_creator(new_state)

    new_state
  end

  # Get turnID of the round (position)
  defp get_position(round, total_players) do
    rem(round, total_players)
  end

  # Get ValidatorID of round creator from PositionID or turnID
  defp get_round_creator(ets_players, position) do
    case :ets.slot(ets_players, position) do
      [object] -> object
      [] -> raise RuntimeError, "Error not there round creator"
    end
  end

  defp get_total_players(ets_players) do
    :ets.info(ets_players, :size)
  end

  # Connect to nodes without exceeded max peers to connect
  # Return number of new connections. Zero in case not connect to new nodes
  defp connect_to_peers(ets_players, total_players) do
    t =
      Task.async(fn ->
        take = min(@max_peers_conn - NetworkNode.count(), total_players)
        players_connected = NetworkNode.list()

        if take > 0 do
          :ets.tab2list(ets_players)
          |> Kernel.--(players_connected)
          |> Enum.take_random(take)
          |> Enum.reduce_while(0, fn {_id, node}, acc ->
            if acc < take do
              case NetworkNode.connect(node) do
                true -> {:cont, acc + 1}
                false -> {:cont, acc}
              end
            else
              {:halt, acc}
            end
          end)
        else
          0
        end
      end)

    Task.await(t, :infinity)
  end

  # connect to round creator, send candidate if exists and question round creator about msg_round
  defp sync_to_round_creator(%{
         rcid: node_id,
         rc_node: node,
         vid: vid,
         round_id: round_id,
         candidate: candidate
       }) do
    if vid != node_id do
      case NetworkNode.connect(node, retry: 2, reconnect: false) do
        true ->
          NetworkNode.cast(node_id, "msg_block", candidate)
          {:ok, response} = NetworkNode.call(node_id, "get_msg_round", round_id)

          # Disconnect if count is mayor than to max_peers_conn
          if NetworkNode.count() > @max_peers_conn do
            NetworkNode.disconnect(node_id)
          end

          send(self(), {"msg_round", response})

        false ->
          Logger.warning("It was not possible to connect to the round creator")
      end
    else
      :none
    end
  end
end
