defmodule RoundManager do
  use GenServer, restart: :transient
  alias Ippan.{NetworkNode, ClusterNode, Block, Round, TxHandler, Validator}
  alias Phoenix.PubSub

  import Ippan.Block, only: [decode_file!: 1]

  require SqliteStore
  require Ippan.TxHandler
  require BalanceStore
  require BigNumber
  require Logger

  @miner_pool :miner_pool
  @pubsub :cluster
  @wait_time 4_000
  @token Application.compile_env(:ipncore, :token)
  @timeout 15_000
  @block_interval Application.compile_env(:ipncore, :block_interval)
  @max_peers_conn Application.compile_env(:ipncore, :max_peers_conn)

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    IO.inspect("Init RoundManager")

    vid = :persistent_term.get(:vid)
    main = {conn = :persistent_term.get(:asset_conn), stmts = :persistent_term.get(:asset_stmt)}

    ets_players =
      ets_start(:players, [:ordered_set, :public, read_concurrency: true, write_concurrency: true])

    ets_votes =
      ets_start(:votes, [:set, :public, read_concurrency: true, write_concurrency: true])

    ets_candidates =
      ets_start(:candidates, [:set, :public, read_concurrency: true, write_concurrency: true])

    [round_id, round_hash] =
      SqliteStore.fetch(conn, stmts, "last_round", [], [-1, nil])

    [block_id] =
      SqliteStore.fetch(conn, stmts, "last_block_id", [], [-1])

    [block_height, block_hash] =
      SqliteStore.fetch(conn, stmts, "last_block_created", [vid], [-1, nil])

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
       block_id: block_id + 1,
       block_height: block_height + 1,
       block_hash: block_hash,
       candidate: nil,
       dets: :persistent_term.get(:dets_balance),
       main: main,
       miner_pool: miner_pool_pid,
       players: ets_players,
       votes: ets_votes,
       candidates: ets_candidates,
       turn: false,
       position: nil,
       rcid: nil,
       rc_node: nil,
       round_id: round_id + 1,
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
  def handle_info(
        :timeout,
        %{main: {conn, stmts}, players: ets_players, round_id: round_id, rcid: rcid, vid: vid} =
          state
      ) do
    IO.puts("Round ##{round_id} Timeout #{rcid}")

    if rcid != vid do
      pid = self()

      t = Task.async(fn -> build_round_from_messages(pid, state) end)

      if Task.await(t, :infinity) == nil do
        # delete validator
        :done = SqliteStore.step(conn, stmts, "delete_validator", [rcid])
        SqliteStore.sync(conn)

        # delete player
        :ets.delete(ets_players, rcid)
        NetworkNode.disconnect(rcid)
        total_players = get_total_players(ets_players)

        # send event
        PubSub.local_broadcast_from(@pubsub, pid, "validator", %{
          "event" => "validator.delete",
          "data" => rcid
        })

        {:noreply, %{state | total: total_players}, {:continue, :next}}
      else
        {:noreply, state, {:continue, :next}}
      end
    else
      {:noreply, state, {:continue, :next}}
    end
  end

  # Check if there are transactions to create local block
  def handle_info(
        :mine,
        %{
          bRef: bRef,
          candidate: nil,
          vid: vid,
          block_height: block_height,
          block_hash: block_hash
        } = state
      ) do
    :timer.cancel(bRef)
    pid = self()

    spawn_link(fn ->
      result = BlockHandler.generate_files(vid, block_height, block_hash)
      {:ok, bRef} = :timer.send_after(@block_interval, :mine)
      GenServer.cast(pid, {:candidate, result, bRef})
    end)

    {:noreply, state}
  end

  def handle_info(:mine, %{bRef: bRef} = state) do
    :timer.cancel(bRef)
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:noreply, %{state | bRef: bRef}, :hibernate}
  end

  def handle_info(
        {
          "msg_round",
          msg_round = %{
            id: id,
            blocks: blocks,
            creator: creator_id,
            hash: hash,
            signature: signature,
            prev: prev
          },
          node_id
        },
        %{
          players: ets_players,
          votes: ets_votes,
          rcid: rcid,
          round_id: round_id,
          tRef: tRef,
          vid: vid
        } =
          state
      )
      when vid != node_id and
             vid != creator_id do
    Logger.debug(inspect(msg_round))
    limit = EnvStore.round_blocks()

    with true <- creator_id == rcid,
         true <- limit >= length(blocks),
         [{_, player}] <- :ets.lookup(ets_players, creator_id),
         hashes <- Enum.map(blocks, & &1.hash),
         true <- hash == Round.compute_hash(id, prev, creator_id, hashes),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         true <- :ets.insert_new(ets_votes, {{id, node_id, :vote}, nil}) do
      count = :ets.update_counter(ets_votes, {id, hash}, {3, 1}, {{id, hash}, msg_round, 1})

      if id == round_id do
        n = NetworkNode.count()

        cond do
          count == div(n, 2) + 1 ->
            IO.inspect("vote ##{id}")
            pid = self()
            :timer.cancel(tRef)

            spawn_link(fn ->
              build_round_from_messages(pid, Map.put(state, :message, msg_round))
            end)

          true ->
            :ok
        end
      end

      # Replicate message to rest of nodes
      NetworkNode.broadcast_except(%{"event" => "msg_round", "data" => msg_round}, [
        node_id,
        creator_id
      ])
    end

    {:noreply, state}
  end

  def handle_info(
        {
          "msg_block",
          block = %{
            "creator" => creator_id,
            "height" => height,
            "hash" => hash,
            "signature" => signature,
            "prev" => prev,
            "hashfile" => hashfile,
            "timestamp" => timestamp
          },
          _node_id
        },
        %{main: {conn, stmts}, candidates: ets_candidates, players: ets_players} = state
      ) do
    with [{_, player}] <- :ets.lookup(ets_players, creator_id),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         true <- Block.compute_hash(creator_id, height, prev, hashfile, timestamp),
         true <-
           height == 1 + SqliteStore.one(conn, stmts, "last_block_height_created", [creator_id]) do
      block =
        block
        |> Map.take(Block.fields())
        |> MapUtil.to_atoms()

      :ets.insert(ets_candidates, {{creator_id, height}, block})
    end

    {:noreply, state}
  end

  def handle_info(
        %{"event" => "validator.new", "data" => validator},
        %{players: ets_players} = state
      ) do
    # add player
    :ets.insert(ets_players, Validator.to_tuple(validator))
    total_players = get_total_players(ets_players)

    {:noreply, %{state | total: total_players}}
  end

  def handle_info(
        %{"event" => "validator.delete", "data" => validator_id},
        %{players: ets_players} = state
      ) do
    # delete player
    :ets.delete(ets_players, validator_id)
    NetworkNode.disconnect(validator_id)
    total_players = get_total_players(ets_players)
    {:noreply, %{state | total: total_players}}
  end

  def handle_info(msg, state) do
    Logger.debug("RoundManager - handle_info: " <> inspect(msg))
    {:noreply, state}
  end

  @impl true
  def handle_call(
        :candidate,
        _from,
        %{
          bRef: bRef,
          candidate: nil,
          vid: vid,
          block_hash: block_hash,
          block_height: block_height
        } = state
      ) do
    :timer.cancel(bRef)
    result = BlockHandler.generate_files(vid, block_height, block_hash)
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:reply, result, %{state | bRef: bRef, candidate: result}, :hibernate}
  end

  def handle_call(:candidate, _from, %{bRef: bRef, candidate: candidate} = state) do
    :timer.cancel(bRef)
    {:ok, bRef} = :timer.send_after(@block_interval, :mine)
    {:reply, candidate, %{state | bRef: bRef}, :hibernate}
  end

  @impl true
  # Process round
  def handle_cast(
        {:complete, round},
        %{
          candidates: ets_candidates,
          main: {conn, stmts},
          dets: dets,
          block_id: block_id,
          round_id: round_id,
          vid: vid,
          votes: ets_votes
        } = state
      ) do
    Logger.debug("[completed] Round ##{round_id} | #{Base.encode16(round.hash)}")

    # Clear round-message-votes and block-candidates
    :ets.select_delete(ets_votes, [{{{round_id, :_}, :_, :_}, [], [true]}])
    :ets.select_delete(ets_votes, [{{{round_id, :_, :_}, :_}, [], [true]}])
    :ets.delete_all_objects(ets_candidates)

    [block_height, block_hash] =
      SqliteStore.fetch(conn, stmts, "last_block_created", [vid], [-1, nil])

    # IO.inspect("select_delete votes: #{c}")

    # save all round
    spawn_link(fn ->
      SqliteStore.sync(conn)
      DetsPlus.commit(dets)
      DetsPlus.sync(dets)
      # replicate data to cluster nodes
      ClusterNode.broadcast(%{"event" => "round.new", "data" => round})
    end)

    {:noreply,
     %{
       state
       | block_height: block_height + 1,
         block_hash: block_hash,
         candidate: nil,
         round_id: round.id + 1,
         round_hash: round.hash,
         block_id: block_id + length(round.blocks)
     }, {:continue, :next}}
  end

  def handle_cast({:candidate, candidate, bRef}, %{candidate: candidate} = state) do
    case candidate do
      nil ->
        {:noreply, %{state | bRef: bRef}}

      candidate ->
        {:noreply, %{state | bRef: bRef, candidate: candidate}}
    end
  end

  def handle_cast(msg, state) do
    Logger.debug("RoundManager - handle_cast: " <> inspect(msg))
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %{
        players: ets_players,
        candidates: ets_candidates,
        votes: ets_votes,
        miner_pool: miner_pool_pid
      }) do
    :ets.delete(ets_players)
    :ets.delete(ets_votes)
    :ets.delete(ets_candidates)
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

  defp ets_start(name, opts) do
    case :ets.whereis(name) do
      :undefined -> :ets.new(name, opts)
      table -> table
    end
  end

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

  defp build_round_from_messages(
         pid,
         %{
           block_id: block_id,
           round_id: round_id,
           round_hash: round_hash,
           main: {conn, stmts},
           dets: dets,
           rcid: rcid,
           miner_pool: pool,
           votes: ets_votes
         } = map
       ) do
    msg_round =
      case Map.get(map, :message) do
        nil ->
          # Choose msg round with more votes
          # :ets.fun2ms(fn {{id, _hash}, _, _} = x when id == 1 -> x end)
          :ets.select(ets_votes, [{{{:"$1", :"$2"}, :_, :_}, [{:==, :"$1", round_id}], [:"$_"]}])
          |> Enum.sort(fn {_, _, a}, {_, _, b} -> a >= b end)
          |> List.first()
          |> case do
            nil -> nil
            x -> elem(x, 1)
          end

        message ->
          message
      end

    if msg_round != nil do
      blocks =
        msg_round.blocks

      {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)

      creator =
        SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", rcid, Validator)

      hash_count = length(hashes)

      # Tasks to create blocks
      result =
        Enum.with_index(blocks, fn element, index -> {block_id + index, element} end)
        |> Enum.map(fn {id, block} ->
          Task.async(fn ->
            :poolboy.transaction(
              pool,
              fn pid -> MinerWorker.mine(pid, Map.put(block, :id, id), creator, round_id) end,
              :infinity
            )
          end)
        end)
        |> Enum.map(fn t -> Task.await(t, :infinity) end)

      IO.inspect(result)

      # Count Blocks and txs rejected
      {blocks, txs_rejected} =
        Enum.reduce(result, {[], 0}, fn x, {acc, acc_txr} ->
          case x do
            {:ok, block} -> {acc ++ [block], acc_txr + block.rejected}
            :error -> {acc, acc_txr}
          end
        end)

      block_count = length(blocks)

      if (hash_count > 0 and block_count > 0) or hash_count == block_count do
        # Run deferred txs
        TxHandler.run_deferred_txs(conn, stmts, dets)

        # Calculate reward
        reward = Round.reward(tx_count, txs_rejected, size)

        if reward > 0 do
          balance_key = BalanceStore.gen_key(creator.owner, @token)
          BalanceStore.income(dets, balance_key, reward)
        end

        # Run jackpot and events
        jackpot_amount =
          if rem(round_id, 100) == 0 do
            last_block_id = block_id + block_count
            run_jackpot(conn, stmts, dets, round_id, last_block_id, round_hash, reward)
          else
            0
          end

        # save round
        round = %{
          id: round_id,
          creator: msg_round.creator,
          hash: msg_round.hash,
          prev: round_hash,
          signature: msg_round.signature,
          coinbase: reward + jackpot_amount,
          count: block_count,
          tx_count: tx_count,
          size: size,
          blocks: blocks,
          extra: nil
        }

        # IO.inspect(round)

        :done = SqliteStore.step(conn, stmts, "insert_round", Round.to_list(round))

        GenServer.cast(pid, {:complete, round})
      else
        send(pid, :timeout)
      end
    end
  end

  defp build_round(pid, %{
         block_id: block_id,
         round_id: round_id,
         round_hash: round_hash,
         candidates: ets_candidates,
         main: {conn, stmts},
         dets: dets,
         miner_pool: pool_pid,
         candidate: candidate,
         vid: creator_id
       }) do
    # Time to wait messages (msg_block) to arrived
    if is_nil(candidate) do
      :timer.sleep(@wait_time)
    end

    candidate =
      if candidate do
        candidate
      else
        get_candidate(pid)
      end

    candidate = if is_nil(candidate), do: [], else: [candidate]

    blocks =
      candidate ++
        :ets.tab2list(ets_candidates)

    {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)
    creator = :persistent_term.get(:validator)
    hash = Round.compute_hash(round_id, round_hash, creator_id, hashes)
    hash_count = length(hashes)

    {:ok, signature} = Cafezinho.Impl.sign(hash, :persistent_term.get(:privkey))

    # pre-build
    pre_round = %{
      id: round_id,
      blocks: hashes,
      creator: creator_id,
      hash: hash,
      signature: signature,
      prev: round_hash
    }

    # send message
    NetworkNode.broadcast(%{"event" => "msg_round", "data" => pre_round})

    # Tasks to create blocks
    result =
      Enum.with_index(blocks, fn element, index -> {block_id + index, element} end)
      |> Enum.map(fn {id, block} ->
        Task.async(fn ->
          :poolboy.transaction(
            pool_pid,
            fn pid ->
              MinerWorker.mine(pid, Map.put(block, :id, id), creator, round_id)
            end,
            :infinity
          )
        end)
      end)
      |> Enum.map(&Task.await(&1, :infinity))

    IO.inspect(result)

    # Count Blocks and txs rejected
    {blocks, txs_rejected} =
      Enum.reduce(result, {[], 0}, fn x, {acc, acc_txr} ->
        case x do
          {:ok, block} -> {acc ++ [block], acc_txr + block.rejected}
          :error -> {acc, acc_txr}
        end
      end)

    block_count = length(blocks)

    if (hash_count > 0 and block_count > 0) or hash_count == block_count do
      # Run deferred txs
      TxHandler.run_deferred_txs(conn, stmts, dets)

      # Calculate reward
      reward = Round.reward(tx_count, txs_rejected, size)

      if reward > 0 do
        balance_key = BalanceStore.gen_key(creator.owner, @token)
        BalanceStore.income(dets, balance_key, reward)
      end

      # Run jackpot and events
      jackpot_amount =
        if rem(round_id, 100) == 0 do
          last_block_id = block_id + block_count
          run_jackpot(conn, stmts, dets, round_id, last_block_id, round_hash, reward)
        else
          0
        end

      # save round
      round = %{
        id: round_id,
        creator: creator_id,
        hash: hash,
        prev: round_hash,
        signature: signature,
        coinbase: reward + jackpot_amount,
        count: block_count,
        tx_count: tx_count,
        size: size,
        blocks: blocks,
        extra: nil
      }

      # IO.inspect(round)

      :done = SqliteStore.step(conn, stmts, "insert_round", Round.to_list(round))

      GenServer.cast(pid, {:complete, round})
    else
      SqliteStore.rollback(conn)
      DetsPlus.rollback(dets)
      send(pid, :timeout)
    end
  end

  defp run_jackpot(_conn, _stmts, _dets, _round_id, _block_id, _round_hash, 0), do: 0

  defp run_jackpot(_conn, _stmts, _dets, _round_id, _block_id, nil, _reward), do: 0

  defp run_jackpot(conn, stmts, dets, round_id, block_id, round_hash, reward) do
    n = BigNumber.to_int(round_hash)
    dv = min(block_id + 1, 20_000)
    b = rem(n, dv) + if(block_id >= 20_000, do: block_id, else: 0)

    case SqliteStore.fetch(conn, stmts, "get_block", [b]) do
      nil ->
        0

      block_list ->
        block = Block.list_to_map(block_list)
        tx_count = block.count

        cond do
          tx_count > 0 ->
            tx_n = rem(n, tx_count)
            path = Block.block_path(block.creator, block.height)
            {:ok, content} = File.read(path)
            %{"data" => data} = decode_file!(content)

            winner_id =
              case Enum.at(data, tx_n) do
                [_hash, _type, account_id, _args, _timestamp, _size] ->
                  balance_key = BalanceStore.gen_key(account_id, @token)
                  BalanceStore.income(dets, balance_key, reward)
                  account_id

                [_hash, _type, _arg_key, account_id, _args, _timestamp, _size] ->
                  balance_key = BalanceStore.gen_key(account_id, @token)
                  BalanceStore.income(dets, balance_key, reward)
                  account_id
              end

            :done =
              SqliteStore.step(conn, stmts, "insert_jackpot", [
                round_id,
                winner_id,
                reward
              ])

            reward

          true ->
            0
        end
    end
  end

  # Check turn of the round and connect to round creator, check another connections
  defp get_state_after_check(
         %{players: ets_players, round_id: round_id, total: total_players, vid: vid} = state
       ) do
    position = get_position(round_id, total_players)
    {rcid, rc_node} = get_round_creator(ets_players, position)
    turn = rcid == vid

    IO.puts(
      "RCID: " <>
        inspect(rcid) <>
        " | Position: " <>
        inspect(position) <>
        " | MyTurn: " <> inspect(turn)
    )

    new_state = %{state | position: position, rcid: rcid, rc_node: rc_node, turn: turn}

    connect_to_peers(ets_players, vid, total_players)
    sync_to_round_creator(new_state)

    new_state
  end

  # Get turnID of the round (position)
  defp get_position(_round, 0), do: 0

  defp get_position(round, total_players) do
    rem(round, total_players)
  end

  # Get ValidatorID of round creator from PositionID or turnID
  defp get_round_creator(ets_players, position) do
    case :ets.slot(ets_players, position) do
      [object] -> object
      _ -> raise RuntimeError, "Error not there round creator"
    end
  end

  defp get_total_players(ets_players) do
    :ets.info(ets_players, :size)
  end

  # Connect to nodes without exceeded max peers to connect
  # Return number of new connections. Zero in case not connect to new nodes
  defp connect_to_peers(ets_players, vid, total_players) do
    t =
      Task.async(fn ->
        take = min(@max_peers_conn - NetworkNode.count(), total_players - 1)
        players_connected = NetworkNode.list()

        if take > 0 do
          :ets.tab2list(ets_players)
          |> Enum.filter(fn {id, _} = x -> id != vid and x not in players_connected end)
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
      case NetworkNode.connect(node) do
        true ->
          if candidate do
            NetworkNode.cast(node_id, "msg_block", candidate)
          end

          case NetworkNode.call(node_id, "get_round", round_id) do
            {:ok, response} when is_map(response) ->
              send(self(), {"msg_round", response, node_id})

              # Disconnect if count is mayor than to max_peers_conn
              if NetworkNode.count() > @max_peers_conn do
                NetworkNode.disconnect(node_id)
              end

            _ ->
              :ok
          end

        false ->
          Logger.warning("It was not possible to connect to the round creator")
      end
    else
      :none
    end
  end
end
