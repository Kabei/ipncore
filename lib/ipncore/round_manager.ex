defmodule RoundManager do
  use GenServer

  alias Ippan.{
    BlockHandler,
    NetworkNodes,
    ClusterNodes,
    Block,
    Round,
    Token,
    TxHandler,
    Validator,
    Round
  }

  alias Phoenix.PubSub
  import Ippan.Block, only: [decode_file!: 1]
  require Ippan.{Block, Round, Token, TxHandler, Validator}
  require Sqlite
  require BalanceStore
  require BigNumber
  require Logger

  @app Mix.Project.config()[:app]
  @miner_pool :miner_pool
  @pubsub :pubsub
  @validator_topic "validator"
  @token Application.compile_env(@app, :token)
  @timeout Application.compile_env(@app, :round_timeout)
  @max_peers_conn Application.compile_env(@app, :max_peers_conn)
  @maintenance Application.compile_env(@app, :maintenance)
  @min_time_to_request 0
  @max_time_to_request 4_000

  def start_link(args) do
    case System.get_env("test") do
      nil ->
        GenServer.start_link(__MODULE__, args, name: __MODULE__)

      _ ->
        :ignore
    end
  end

  @impl true
  def init(_args) do
    IO.puts("RoundManager init")

    vid = :persistent_term.get(:vid)
    db_ref = :persistent_term.get(:main_conn)

    ets_players =
      ets_start(:players, [:ordered_set, :public])

    ets_votes =
      ets_start(:votes, [:set, :public])

    ets_candidates =
      ets_start(:candidates, [:set, :public])

    %{id: round_id, hash: round_hash} = Round.last()

    block_id = Sqlite.one("last_block_id", [], -1)

    current_block_id = block_id + 1
    current_round_id = round_id + 1

    # Subscribe
    PubSub.subscribe(@pubsub, "validator")
    # PubSub.subscribe(@pubsub, "env")

    # Get all players
    players = Sqlite.all("get_players")
    total_players = length(players)

    miner_pool_pid = start_miner_pool()

    # Fill data to table of players
    for v <- players do
      :ets.insert(ets_players, Validator.list_to_tuple(v))
    end

    {:ok,
     %{
       block_id: current_block_id,
       balance: DetsPlux.get(:balance),
       db_ref: db_ref,
       status: :startup,
       miner_pool: miner_pool_pid,
       players: ets_players,
       votes: ets_votes,
       candidates: ets_candidates,
       turn: false,
       position: nil,
       rcid: nil,
       rc_node: nil,
       round_id: current_round_id,
       round_hash: round_hash,
       total: total_players,
       rRef: nil,
       tRef: nil,
       ttr: @min_time_to_request,
       vid: vid,
       vote_round_id: current_round_id
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(:next, %{status: :startup, rRef: rRef, tRef: tRef} = init_state) do
    IO.puts("RM startup")
    :persistent_term.put(:status, :startup)
    :timer.cancel(rRef)
    :timer.cancel(tRef)

    state = RoundTask.get_state_after_check(init_state)

    # Start RoundSync process if not running
    RoundSync.start_link(%{
      block_id: state.block_id,
      db_ref: state.db_ref,
      balance: state.balance,
      miner_pool: state.miner_pool,
      pid: self()
    })

    {:noreply, state, :hibernate}
  end

  def handle_continue(
        :next,
        %{rRef: rRef, tRef: tRef, ttr: time_to_request} = state
      ) do
    IO.puts("RM next")
    :timer.cancel(rRef)
    :timer.cancel(tRef)
    new_state = RoundTask.get_state_after_check(state)

    if new_state.turn do
      spawn_build_local_round(new_state)
      {:ok, tRef} = :timer.send_after(@timeout, :timeout)

      {:noreply, %{new_state | tRef: tRef}, :hibernate}
    else
      {:ok, tRef} = :timer.send_after(@timeout, :timeout)
      {:ok, rRef} = :timer.send_after(time_to_request, :request)

      if time_to_request != @min_time_to_request do
        spawn_send_block(new_state)
      end

      {:noreply, %{new_state | rRef: rRef, tRef: tRef}, {:continue, :retrieve}}
    end
  end

  def handle_continue(:retrieve, state = %{votes: ets_votes}) do
    retrieve_messages(ets_votes, state.round_id)
    {:noreply, state}
  end

  @impl true
  def handle_info(:request, state) do
    IO.inspect("request")

    case RoundTask.sync_to_round_creator(state) do
      :error ->
        IO.inspect("nothing")
        {:noreply, %{state | ttr: @max_time_to_request}}

      {:ok, response, node_id} ->
        IO.inspect("get data request")
        GenServer.cast(self(), {"msg_round", response, node_id})

        {:noreply, %{state | ttr: @min_time_to_request}}
    end
  end

  def handle_info(
        :timeout,
        %{
          round_id: round_id,
          round_hash: prev_hash,
          # db_ref: db_ref,
          rcid: rcid,
          vid: vid
        } = state
      ) do
    Logger.warning("Round ##{round_id} Timeout | ID: #{rcid}")

    case check_votes(state) do
      nil ->
        IO.puts("no votes")

        pid = self()

        # case RoundTask.sync_to_round_creator(state) do
        #   {:ok, response, node_id} ->
        #     GenServer.cast(pid, {"msg_round", response, node_id})

        #   _error ->
        #   end
        round_nulled = Round.cancel(round_id, prev_hash, rcid, 1)
        GenServer.cast(pid, {"msg_round", round_nulled, vid})

        {:noreply, %{state | ttr: @min_time_to_request}, :hibernate}

      # IO.inspect(r)

      # case r do
      #   x when x in [:error, nil] ->
      #     pid = self()

      #     incomplete(round_nulled, pid, db_ref, true)
      #     GenServer.cast(pid, {"msg_round", round_nulled})

      #     {:noreply, state, :hibernate}

      # if total_players > 1 do
      # else
      #   {:ok, tRef} = :timer.send_after(@timeout, :timeout)
      #   {:noreply, %{state | tRef: tRef}, :hibernate}
      # end

      #   _ ->
      #     {:noreply, state, :hibernate}
      # end

      message ->
        IO.puts("sync_to_round_creator #{inspect(message)}")
        spawn_build_foreign_round(state, message)
        {:noreply, %{state | ttr: @min_time_to_request}, :hibernate}
    end
  end

  def handle_info(
        %{"event" => "validator.active", "data" => %{"id" => id, "active" => active}},
        %{db_ref: db_ref, players: ets_players} = state
      ) do
    if active do
      # add player
      :ets.insert(ets_players, {id, Validator.get(id)})
    else
      # remove player
      :ets.delete(ets_players, id)
    end

    total_players = get_total_players(ets_players)

    {:noreply, %{state | total: total_players}}
  end

  def handle_info(
        %{"event" => "validator.leave", "data" => validator_id},
        %{players: ets_players} = state
      ) do
    # delete player
    :ets.delete(ets_players, validator_id)
    NetworkNodes.disconnect_all(validator_id)
    total_players = get_total_players(ets_players)
    {:noreply, %{state | total: total_players}}
  end

  def handle_info(
        %{"event" => "validator.update", "data" => %{"id" => validator_id}},
        %{db_ref: db_ref, players: ets_players} = state
      ) do
    # update player
    new_data = Validator.get(validator_id)
    :ets.insert(ets_players, {validator_id, new_data})
    total_players = get_total_players(ets_players)
    {:noreply, %{state | total: total_players}}
  end

  def handle_info(msg, state) do
    Logger.warning("RoundManager - handle_info: " <> inspect(msg))
    {:noreply, state}
  end

  @impl true
  # Process round
  def handle_cast(
        {
          :complete,
          round = %{id: the_round_id, hash: hash, blocks: blocks}
        },
        %{
          candidates: ets_candidates,
          block_id: block_id,
          round_id: round_id,
          votes: ets_votes
        } = state
      ) do
    # next = the_round_id == round_id
    Logger.debug("[completed] Round ##{the_round_id} | #{Base.encode16(hash)}")

    # Clear round-message-votes and block-candidates
    delete_old_votes(ets_votes, ets_candidates, round_id)

    # replicate data to cluster nodes
    ClusterNodes.broadcast(%{"event" => "round.new", "data" => round})

    # Set last local height and prev hash and reset timer
    BlockTimer.complete(blocks)
    next_id = the_round_id + 1

    {:noreply,
     %{
       state
       | block_id: block_id + length(round.blocks),
         round_id: next_id,
         vote_round_id: next_id,
         round_hash: round.hash
     }, {:continue, :next}}
  end

  def handle_cast(
        {:incomplete,
         %{id: round_id, hash: hash, creator: creator_id, status: round_status} = round_nulled},
        %{
          candidates: ets_candidates,
          players: ets_players,
          round_id: round_id,
          status: _status,
          total: total_players,
          votes: ets_votes
        } =
          state
      ) do
    # next = status == :synced
    Logger.debug("[Incomplete] Round ##{round_id} | Status: #{round_status}")

    delete_old_votes(ets_votes, ets_candidates, round_id)

    # Delete player
    if total_players > 2 and round_status in 1..2 do
      :ets.delete(ets_players, creator_id)
      NetworkNodes.disconnect_all(creator_id)
    end

    total_players = get_total_players(ets_players)

    # replicate data to cluster nodes
    ClusterNodes.broadcast(%{"event" => "round.new", "data" => round_nulled})

    # send event
    PubSub.local_broadcast_from(@pubsub, self(), "validator", %{
      "event" => "validator.leave",
      "data" => creator_id
    })

    next_id = round_id + 1

    {:noreply,
     %{state | round_id: next_id, vote_round_id: next_id, round_hash: hash, total: total_players},
     {:continue, :next}}
  end

  def handle_cast(
        {
          "msg_round",
          msg_round = %{
            id: id,
            blocks: blocks,
            creator: creator_id,
            hash: hash,
            status: status,
            signature: signature,
            timestamp: timestamp,
            prev: prev
          },
          node_id
        },
        %{
          db_ref: db_ref,
          players: ets_players,
          votes: ets_votes,
          round_id: round_id,
          round_hash: round_hash,
          rcid: rcid,
          status: :synced,
          # total: total_players,
          # vid: vid,
          # rRef: rRef,
          vote_round_id: vote_round_id
        } =
          state
      )
      when vote_round_id == id and round_id == vote_round_id and creator_id == rcid do
    Logger.debug(inspect(msg_round))

    cond do
      :ets.member(ets_votes, {id, node_id, :vote}) ->
        {:noreply, state}

      :ets.member(ets_votes, {id, hash}) ->
        IO.puts("#{id} = #{vote_round_id} | single")
        do_vote(msg_round, node_id, state)

      true ->
        with true <- round_hash == prev,
             true <- EnvStore.block_limit() >= length(blocks),
             [{_, player}] <- :ets.lookup(ets_players, creator_id),
             hashes <- Enum.map(blocks, & &1.hash),
             true <- hash == Round.compute_hash(id, prev, creator_id, hashes, timestamp),
             true <-
               (status > 0 and signature == nil) or
                 Cafezinho.Impl.verify(signature, hash, player.pubkey) == :ok,
             true <- blocks_verificacion(blocks, db_ref) do
          IO.puts("#{id} = #{vote_round_id} | first")
          do_vote(msg_round, node_id, state)
        else
          _ ->
            {:noreply, state}
        end
    end
  end

  def handle_cast(
        {"msg_round", msg_round = %{id: id, hash: hash}, node_id},
        %{db_ref: db_ref, vote_round_id: vote_round_id} = state
      )
      when vote_round_id > id do
    case Round.get(id) do
      %{hash: rhash} when rhash == hash ->
        NetworkNodes.cast(node_id, "msg_round", msg_round)
        {:noreply, state}

      _ ->
        {:noreply, state}
    end
  end

  def handle_cast(
        {"msg_round", msg = %{id: id}, _node_id},
        %{round_id: round_id, status: :startup} = state
      ) do
    if round_id >= id do
      RoundSync.add_queue(msg)
    end

    {:noreply, state}
  end

  def handle_cast(
        {"msg_round", msg_round = %{id: id}, node_id},
        state = %{round_id: round_id, votes: ets_votes}
      )
      when id > round_id do
    Logger.debug("Id is high")
    Logger.debug(inspect(msg_round))
    :ets.insert(ets_votes, {{id, node_id, :msg}, msg_round})
    {:noreply, state}
  end

  def handle_cast({"msg_round", msg_round, _node_id}, state) do
    Logger.debug("No match message")
    Logger.debug(inspect(msg_round))
    IO.inspect(state)
    {:noreply, state}
  end

  def handle_cast(
        {"msg_block", block = %{creator: creator_id, height: height}, _node_id},
        state = %{
          db_ref: db_ref,
          candidates: ets_candidates,
          status: :synced
        }
      ) do
    with false <- Block.exists_local?(creator_id, height),
         true <- :ets.info(ets_candidates, :size) < 10,
         true <- Validator.exists?(creator_id),
         true <- block_verificacion(block, db_ref) do
      :ets.insert(ets_candidates, {{creator_id, height}, block})
    end

    {:noreply, state}
  end

  def handle_cast({"msg_block", block, _node_id}, state) do
    IO.inspect("block message no match")
    IO.inspect(block)
    {:noreply, state}
  end

  def handle_cast({:status, status, next}, state = %{db_ref: db_ref}) do
    IO.puts("Set status: #{status} - #{next}")
    :persistent_term.put(:status, status)

    if next do
      # update state
      %{id: round_id, hash: round_hash} = Round.last()
      block_id = Sqlite.one("last_block_id", [], -1)
      next_id = round_id + 1

      new_state = %{
        state
        | block_id: block_id + 1,
          round_id: next_id,
          vote_round_id: next_id,
          round_hash: round_hash,
          status: status
      }

      {:noreply, new_state, {:continue, :next}}
    else
      {:noreply, %{state | status: status}, :hibernate}
    end
  end

  defp do_vote(
         msg_round = %{id: id, creator: creator_id, hash: hash},
         node_id,
         state = %{
           votes: ets_votes,
           rRef: rRef,
           total: total_players,
           vote_round_id: vote_round_id
         }
       ) do
    :ets.insert(ets_votes, {{id, node_id, :vote}, nil})
    count = :ets.update_counter(ets_votes, {id, hash}, {3, 1}, {{id, hash}, msg_round, 0})
    n = NetworkNodes.count()

    is_creator = creator_id == node_id

    if is_creator do
      :timer.cancel(rRef)
    end

    cond do
      count == 1 ->
        # Replicate message to rest of nodes except creator and sender
        # except = if node_id != creator_id, do: [node_id], else: []
        NetworkNodes.broadcast(%{"event" => "msg_round", "data" => msg_round})

      true ->
        NetworkNodes.cast(node_id, "msg_round", msg_round)
    end

    IO.puts("n = #{n} | count = #{count}")

    cond do
      total_players == count or
          count == div(n, 2) + 1 ->
        IO.puts("Vote ##{id}")

        spawn_build_foreign_round(state, msg_round)
        {:noreply, %{state | vote_round_id: vote_round_id + 1}}

      true ->
        {:noreply, state}
    end
  end

  defp delete_old_votes(ets_votes, ets_candidates, round_id) do
    :ets.select_delete(ets_votes, [{{{:"$1", :_, :_}, :_}, [{:"=<", :"$1", round_id}], [true]}])
    :ets.select_delete(ets_votes, [{{{:"$1", :_}, :_, :_}, [{:"=<", :"$1", round_id}], [true]}])
    :ets.delete_all_objects(ets_candidates)
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
    PubSub.unsubscribe(@pubsub, "validator")
    PubSub.unsubscribe(@pubsub, "env")
    :poolboy.stop(miner_pool_pid)
  end

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

  # defp auto_vote(ets_votes, id, hash, vid, msg_round) do
  #   :ets.insert_new(ets_votes, {{id, vid, :vote}, nil})
  #   :ets.update_counter(ets_votes, {id, hash}, {3, 1}, {{id, hash}, msg_round, 0})
  # end

  defp blocks_verificacion([], _db_ref), do: true

  defp blocks_verificacion(blocks, db_ref) when is_list(blocks) do
    Enum.map(blocks, fn block ->
      # Task.async(fn ->
      block_verificacion(block, db_ref)
      # end)
    end)
    # |> Task.await_many(:infinity)
    |> then(fn x ->
      IO.puts("blocks vrf: #{inspect(x)}")
      x
    end)
    |> Enum.all?()
  end

  defp blocks_verificacion(_, _), do: false

  defp block_verificacion(
         %{
           #  creator: creator_id,
           #  height: height,
           hash: _hash,
           signature: _signature,
           prev: _prev,
           filehash: _filehash,
           timestamp: _timestamp
         } = block,
         db_ref
       ) do
    with :ok <- BlockHandler.check(block, db_ref) do
      true
    else
      _ -> false
    end
  end

  defp block_verificacion(_, _), do: false

  defp spawn_build_foreign_round(
         %{
           block_id: block_id,
           round_id: round_id,
           round_hash: prev_hash,
           db_ref: db_ref,
           balance: balance,
           miner_pool: pool_pid,
           rRef: rRef,
           tRef: tRef
         },
         %{creator: creator_id} = msg_round
       ) do
    :timer.cancel(rRef)
    :timer.cancel(tRef)

    pid = self()
    IO.puts("RM: spawn_build_foreign_round #{round_id}")

    spawn_link(fn ->
      creator = Validator.get(creator_id)

      unless Round.exists?(round_id) do
        build_round(
          %{msg_round | prev: prev_hash},
          block_id,
          creator,
          db_ref,
          balance,
          pool_pid,
          pid
        )
      end
    end)
  end

  defp spawn_build_local_round(%{
         block_id: block_id,
         round_id: round_id,
         round_hash: prev_hash,
         candidates: ets_candidates,
         db_ref: db_ref,
         status: status,
         balance: balances,
         miner_pool: pool_pid,
         rcid: rcid,
         total: total_players,
         vid: vid
       }) do
    if rcid == vid do
      IO.puts("RM: build_local_round #{round_id}")

      blocks =
        case status do
          :synced ->
            limit = EnvStore.block_limit()

            BlockTimer.get_block(block_id)
            |> Kernel.++(
              :ets.tab2list(ets_candidates)
              |> Enum.filter(fn {{creator_id, height}, _b} ->
                Block.exists_local?(creator_id, height) == false
              end)
              |> Enum.map(fn {_, b} -> b end)
            )
            |> Enum.take(limit)

          # Time to wait messages (msg_block) to arrived
          _ ->
            []
        end

      creator = Validator.get(vid)
      timestamp = :erlang.system_time(:millisecond)
      {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)
      hash = Round.compute_hash(round_id, prev_hash, creator.id, hashes, timestamp)
      {:ok, signature} = Cafezinho.Impl.sign(hash, :persistent_term.get(:privkey))

      # pre-build
      pre_round = %{
        id: round_id,
        blocks: blocks,
        creator: creator.id,
        hash: hash,
        signature: signature,
        prev: prev_hash,
        timestamp: timestamp,
        status: 0,
        size: size,
        tx_count: tx_count
      }

      # send message pre-build
      NetworkNodes.broadcast(%{"event" => "msg_round", "data" => pre_round})

      if total_players == 1 do
        pid = self()

        spawn_link(fn ->
          build_round(
            pre_round,
            block_id,
            creator,
            db_ref,
            balances,
            pool_pid,
            pid
          )
        end)
      end
    end
  end

  # creator,
  # balance_pid,
  # balance_tx,
  # tx_count,
  # txs_rejected,
  # size,
  # {supply_tx, supply_key, supply, max_supply}
  defmacrop run_reward do
    quote location: :keep do
      computed = Round.calc_reward(var!(tx_count), var!(txs_rejected), var!(size))
      current = TokenSupply.get(var!(supply))
      total = current + computed

      reward =
        cond do
          total > var!(max_supply) and var!(max_supply) > 0 ->
            0

          true ->
            computed
        end

      if reward > 0 do
        BalanceStore.income(
          var!(balance_pid),
          var!(balance_tx),
          var!(creator).owner,
          @token,
          reward
        )
      end

      reward
    end
  end

  @spec build_round(map, pos_integer, map, reference(), pid, pid, pid, boolean()) ::
          {:ok, term} | :error
  def build_round(
        %{
          id: round_id,
          blocks: blocks,
          creator: creator_id,
          prev: prev_hash,
          status: status,
          signature: signature,
          timestamp: timestamp
        } = map,
        block_id,
        creator,
        db_ref,
        balance_pid,
        pool_pid,
        pid,
        verify_block \\ true,
        rm_notify \\ true
      ) do
    unless Round.null?(map) do
      block_count = length(blocks)
      IO.puts("Bulding Round: ##{round_id} | Creator: #{creator_id} | Blocks: #{block_count}")

      # IO.inspect(map)

      {hash, tx_count, size} =
        if Map.get(map, :hash) do
          {map.hash, map.tx_count, map.size}
        else
          {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)
          hash = Round.compute_hash(round_id, prev_hash, creator.id, hashes, timestamp)
          {hash, tx_count, size}
        end

      # Tasks to create blocks
      result =
        Enum.with_index(blocks, fn element, index -> {block_id + index, element} end)
        |> Enum.map(fn {id, block} ->
          Task.async(fn ->
            :poolboy.transaction(
              pool_pid,
              fn worker ->
                MinerWorker.mine(worker, Map.put(block, :id, id), creator, round_id, verify_block)
              end,
              :infinity
            )
          end)
        end)
        |> Task.await_many(:infinity)

      # IO.puts("MinerWorker: " <> inspect(result))

      # Count Blocks and txs rejected
      {new_blocks, blocks_approved_count, txs_rejected} =
        Enum.reduce(result, {[], 0, 0}, fn x, {acc, acc_ba, acc_txr} ->
          case x do
            {:ok, block} -> {acc ++ [block], acc_ba + 1, acc_txr + block.rejected}
            {:error, block} -> {acc ++ [block], acc_ba, acc_txr + block.rejected}
          end
        end)

      if blocks_approved_count > 0 or block_count == blocks_approved_count do
        balance_tx = DetsPlux.tx(:balance)
        # Run deferred txs
        TxHandler.run_deferred_txs()

        # Calculate reward
        reward_task =
          Task.async(fn ->
            # Get info native token and current supply
            %{max_supply: max_supply} = Token.get(@token)
            supply = TokenSupply.new(@token)
            run_reward()
          end)

        # Run jackpot and events
        jackpot_task =
          Task.async(fn ->
            run_jackpot(
              db_ref,
              balance_pid,
              balance_tx,
              round_id,
              prev_hash,
              block_id + block_count
            )
          end)

        reward = Task.await(reward_task, :infinity)
        jackpot = Task.await(jackpot_task, :infinity)

        # save round
        round = %{
          id: round_id,
          creator: creator_id,
          hash: hash,
          prev: prev_hash,
          signature: signature,
          count: block_count,
          tx_count: tx_count,
          size: size,
          status: 0,
          timestamp: timestamp,
          blocks: new_blocks,
          extra: nil,
          reward: reward,
          # extra data
          jackpot: jackpot
        }

        :done = Round.to_list(round) |> Round.insert()

        run_maintenance(round_id, db_ref)

        # update stats
        stats = Stats.new()
        Stats.incr(stats, "blocks", block_count)
        Stats.incr(stats, "txs", tx_count)
        Stats.put(stats, "last_round", round_id)
        Stats.put(stats, "last_hash", hash)

        # save all round
        RoundCommit.sync(db_ref, tx_count)

        fun = :persistent_term.get(:last_fun, nil)

        if fun do
          :persistent_term.erase(:last_fun)
          fun.()
        end

        if rm_notify do
          GenServer.cast(pid, {:complete, round})
        end

        {:ok, round}
      else
        round_nulled =
          Round.cancel(round_id, prev_hash, creator_id, 2)

        incomplete(round_nulled, pid, db_ref, rm_notify)
      end
    else
      round_nulled =
        Round.cancel(round_id, prev_hash, creator_id, status)

      incomplete(round_nulled, pid, db_ref, rm_notify)
    end
  end

  defp incomplete(
         %{creator: creator_id, id: round_id, status: status} =
           round_nulled,
         pid,
         db_ref,
         rm_notify
       ) do
    # Reverse changes
    RoundCommit.rollback(db_ref)

    # round nulled
    r = Round.to_list(round_nulled)
    :done = Round.insert(r)

    case status do
      1 ->
        max = EnvStore.max_failures()

        if max != 0 do
          number = Validator.incr_failure(creator_id, 1, round_id)

          if number != nil and rem(number, max) == 0 do
            Validator.disable(creator_id, round_id)

            event = %{
              "event" => "validator.active",
              "data" => %{"id" => creator_id, "active" => false}
            }

            PubSub.broadcast(@pubsub, @validator_topic, event)
          end
        end

      2 ->
        Validator.delete(creator_id)

      _ ->
        nil
    end

    Sqlite.sync(db_ref)

    if rm_notify do
      GenServer.cast(pid, {:incomplete, round_nulled})
    end

    :error
  end

  defp run_jackpot(
         db_ref,
         balances,
         balance_tx,
         round_id,
         round_hash,
         total_blocks
       )
       when rem(round_id, 100) == 0 do
    IO.inspect("jackpot")
    supply = TokenSupply.jackpot()
    amount = TokenSupply.get(supply)

    if amount > 0 do
      n = BigNumber.to_int(round_hash)
      dv = min(total_blocks + 1, 20_000)
      b = rem(n, dv) + if(total_blocks >= 20_000, do: total_blocks, else: 0)

      TokenSupply.put(supply, 0)

      case Block.get(b) do
        nil ->
          {nil, 0}

        block_list ->
          block = Block.list_to_map(block_list)
          tx_count = block.count
          # IO.inspect(tx_count)

          cond do
            tx_count > 0 ->
              tx_n = rem(n, tx_count)
              path = Block.decode_path(block.creator, block.height)
              {:ok, content} = File.read(path)
              %{"data" => data} = decode_file!(content)

              winner_id =
                case Enum.at(data, tx_n) do
                  [_hash, _type, account_id, _nonce, _args, _sig, _size] ->
                    BalanceStore.income(balances, balance_tx, account_id, @token, amount)
                    # Update Token Supply
                    su = TokenSupply.new(@token)
                    TokenSupply.add(su, amount)

                    account_id

                  [_hash, _type, _arg_key, account_id, _nonce, _args, _sig, _size] ->
                    BalanceStore.income(balances, balance_tx, account_id, @token, amount)
                    # Update Token Supply
                    su = TokenSupply.new(@token)
                    TokenSupply.add(su, amount)
                    account_id
                end

              jackpot = [round_id, winner_id, amount]

              :done =
                Sqlite.step("insert_jackpot", jackpot)

              {winner_id, amount}

            true ->
              {nil, 0}
          end
      end
    else
      {nil, 0}
    end
  end

  defp run_jackpot(_, _, _, _, _, _), do: {nil, 0}

  defp run_maintenance(0, _), do: nil

  defp run_maintenance(round_id, db_ref) do
    if rem(round_id, @maintenance) == 0 do
      Sqlite.step("expiry_refund", [round_id])
      Sqlite.step("expiry_domain", [round_id])
    end
  end

  defp get_total_players(ets_players) do
    :ets.info(ets_players, :size)
  end

  defp retrieve_messages(ets_votes, round_id) do
    match =
      [{{{round_id, :_, :msg}, :_}, [], [:"$_"]}]

    Logger.debug("Retrieve messages #{round_id}")

    # :ets.info(ets_votes, :size) |> IO.inspect()

    case :ets.select(ets_votes, match) do
      [] ->
        nil

      data ->
        Logger.debug("some data")
        pid = self()

        Enum.each(data, fn
          {{_, node_id, _msg} = key, msg_round} ->
            :ets.delete(ets_votes, key)
            GenServer.cast(pid, {"msg_round", msg_round, node_id})

          _x ->
            nil
        end)
    end
  end

  defp spawn_send_block(%{
         rcid: node_id,
         rc_node: validator_node
       }) do
    spawn_link(fn ->
      candidate = BlockTimer.get_block()

      if candidate do
        case NetworkNodes.connect(validator_node, retry: 1) do
          false ->
            Logger.warning("It was not possible to connect to the round creator (send_block)")
            :error

          true ->
            NetworkNodes.cast(node_id, "msg_block", candidate)
            # Disconnect if count is greater than max_peers_conn
            if NetworkNodes.count() > @max_peers_conn do
              node = NetworkNodes.info(node_id)
              NetworkNodes.disconnect(node)
            end
        end
      end
    end)
  end

  defp check_votes(%{
         round_id: round_id,
         status: status,
         total: total_players,
         vid: vid,
         votes: ets_votes
       }) do
    # check votes
    n = NetworkNodes.count()

    :ets.select(ets_votes, [{{{:"$1", :"$2"}, :_, :_}, [{:==, :"$1", round_id}], [:"$_"]}])
    |> Enum.sort(fn {_, _, a}, {_, _, b} -> a >= b end)
    |> List.first()
    |> case do
      nil ->
        IO.puts("nil in check_votes")
        nil

      {_, msg_round, count} ->
        IO.inspect("check votes: #{msg_round.id} #{count}")

        cond do
          total_players == count or
              count >= div(n, 2) + 1 ->
            notify =
              :ets.member(ets_votes, {round_id, vid, :vote}) == false

            if status == :synced and notify do
              :ets.insert_new(ets_votes, {{round_id, vid, :vote}, nil})
              # Replicate message to rest of nodes except creator and sender
              NetworkNodes.broadcast(%{"event" => "msg_round", "data" => msg_round})
            end

            msg_round

          # forced_count == true ->
          #   Logger.debug("Forced count: ##{round_id}")
          #   x

          true ->
            nil
        end
    end
  end
end
