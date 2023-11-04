defmodule RoundManager do
  use GenServer
  alias Ippan.{NetworkNodes, ClusterNodes, Block, Round, Token, TxHandler, Validator, Round}
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
  @token Application.compile_env(@app, :token)
  @timeout Application.compile_env(@app, :round_timeout)
  @max_peers_conn Application.compile_env(@app, :max_peers_conn)

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
      ets_start(:players, [:ordered_set, :public, read_concurrency: true, write_concurrency: true])

    ets_votes =
      ets_start(:votes, [:set, :public, read_concurrency: true, write_concurrency: true])

    ets_candidates =
      ets_start(:candidates, [:set, :public, read_concurrency: true, write_concurrency: true])

    [round_id, round_hash] = Sqlite.fetch("last_round", [], [-1, nil])

    block_id = Sqlite.one("last_block_id", [], -1)

    current_block_id = block_id + 1
    current_round_id = round_id + 1

    # Subscribe
    PubSub.subscribe(@pubsub, "validator")
    PubSub.subscribe(@pubsub, "env")

    # Get all players
    players = Sqlite.all("get_players")
    total_players = length(players)

    miner_pool_pid = start_miner_pool()

    # Fill data to table of players
    for v <- players do
      :ets.insert(ets_players, Validator.list_to_tuple(v))
    end

    # start round commit
    # RoundCommit.start_link(nil)

    {:ok,
     %{
       block_id: current_block_id,
       balance: DetsPlux.get(:balance),
       db_ref: db_ref,
       status: :starting,
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
       tRef: nil,
       vid: vid
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(:next, %{status: :starting, tRef: tRef} = init_state) do
    IO.puts("next starting")
    :timer.cancel(tRef)

    new_state =
      case Process.whereis(RoundSync) do
        nil ->
          # start round sync process
          %{round_id: round_id, round_hash: round_hash, miner_pool: miner_pool_pid} =
            state = get_state_after_check(init_state)

          RoundSync.start_link(%{
            round_id: round_id,
            round_hash: round_hash,
            miner_pool: miner_pool_pid,
            pid: self()
          })

          state

        _ ->
          get_state_after_check(init_state)
      end

    {:noreply, %{new_state | status: :starting}, :hibernate}
  end

  def handle_continue(:next, %{tRef: tRef} = state) do
    IO.puts("next")
    :timer.cancel(tRef)
    new_state = get_state_after_check(state)

    if new_state.turn do
      spawn_build_local_round(new_state)

      {:noreply, new_state, :hibernate}
    else
      {:ok, tRef} = :timer.send_after(@timeout, :timeout)
      {:noreply, %{new_state | tRef: tRef}, :hibernate}
    end
  end

  @impl true
  def handle_info(:timeout, %{round_id: round_id, rcid: rcid} = state) do
    IO.puts("Round ##{round_id} Timeout | #{rcid}")

    spawn_build_foreign_round(state)

    {:noreply, state, :hibernate}
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
          db_ref: db_ref,
          players: ets_players,
          votes: ets_votes,
          rcid: rcid,
          round_id: round_id,
          status: status,
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
         false <-
           Enum.any?(blocks, fn block ->
             block_pre_verificacion(block, db_ref, ets_players) == :error
           end),
         hashes <- Enum.map(blocks, & &1.hash),
         true <- hash == Round.compute_hash(id, prev, creator_id, hashes),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         true <- :ets.insert_new(ets_votes, {{id, node_id, :vote}, nil}) do
      count = :ets.update_counter(ets_votes, {id, hash}, {3, 1}, {{id, hash}, msg_round, 0})

      IO.puts("#{id} = #{round_id}")

      if id == round_id do
        n = NetworkNodes.count()
        IO.puts("n = #{n} | count = #{count}")

        cond do
          count == div(n, 2) + 1 ->
            IO.puts("Vote ##{id}")

            if status != :synced do
              GenServer.cast(RoundSync, {:add, msg_round})
            else
              spawn_build_foreign_round(state, msg_round)
            end

          true ->
            nil
        end
      end

      # Replicate message to rest of nodes
      NetworkNodes.broadcast_except(%{"event" => "msg_round", "data" => msg_round}, [
        node_id,
        creator_id,
        vid
      ])
    end

    {:noreply, state}
  end

  def handle_info(
        {"msg_block", block = %{"creator" => creator_id, "height" => height}, _node_id},
        state = %{db_ref: db_ref, candidates: ets_candidates, players: ets_players}
      ) do
    with :ok <- block_pre_verificacion(block, db_ref, ets_players) do
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
    NetworkNodes.disconnect(validator_id)
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
        {:complete, round = %{id: the_round_id, hash: hash}},
        %{
          candidates: ets_candidates,
          db_ref: db_ref,
          vid: vid,
          block_id: block_id,
          round_id: round_id,
          votes: ets_votes
        } = state
      ) do
    next = the_round_id == round_id
    is_some_block_mine = Enum.any?(round.blocks, &(&1.creator == vid))
    Logger.debug("[completed] Round ##{the_round_id} | #{Base.encode16(round.hash)}")

    # Clear round-message-votes and block-candidates
    :ets.select_delete(ets_votes, [{{{round_id, :_}, :_, :_}, [], [true]}])
    :ets.select_delete(ets_votes, [{{{round_id, :_, :_}, :_}, [], [true]}])
    :ets.delete_all_objects(ets_candidates)

    # replicate data to cluster nodes
    ClusterNodes.broadcast(%{"event" => "round.new", "data" => round})

    # save all round
    RoundCommit.sync(db_ref, round.tx_count, is_some_block_mine)

    # Set last local height and prev hash and reset timer
    if next do
      BlockTimer.complete(hash, is_some_block_mine)
    end

    if next do
      {:noreply,
       %{
         state
         | block_id: block_id + length(round.blocks),
           round_id: round.id + 1,
           round_hash: round.hash
       }, {:continue, :next}}
    else
      {:noreply, state, :hibernate}
    end
  end

  def handle_cast(
        {:incomplete, %{id: round_nulled_id} = round_nulled},
        %{
          db_ref: db_ref,
          players: ets_players,
          rcid: rcid,
          round_id: round_id
        } =
          state
      ) do
    next = round_nulled_id == round_id
    Logger.debug("[Incomplete] Round ##{round_nulled_id} | Status: #{round_nulled.status}")

    # Reverse changes
    RoundCommit.rollback(db_ref)

    # round nulled
    :done = Round.insert(Round.to_list(round_nulled))

    # Delete validator
    :done = Validator.delete(rcid)
    Sqlite.sync(db_ref)

    # Delete player
    :ets.delete(ets_players, rcid)
    NetworkNodes.disconnect(rcid)
    total_players = get_total_players(ets_players)

    # replicate data to cluster nodes
    ClusterNodes.broadcast(%{"event" => "round.new", "data" => round_nulled})

    # send event
    PubSub.local_broadcast_from(@pubsub, self(), "validator", %{
      "event" => "validator.delete",
      "data" => rcid
    })

    if next do
      {:noreply, %{state | round_id: round_id + 1, total: total_players}, {:continue, :next}}
    else
      {:noreply, %{state | total: total_players}, :hibernate}
    end
  end

  def handle_cast(
        {:put, %{"id" => round_id, "hash" => round_hash, "block_id" => block_id}},
        state
      ) do
    {:noreply,
     %{
       state
       | round_id: round_id,
         round_hash: round_hash,
         block_id: block_id
     }}
  end

  def handle_cast({:status, status, next}, state) do
    IO.puts("set status: #{status} - #{next}")

    if next do
      {:noreply, %{state | status: status}, {:continue, :next}}
    else
      {:noreply, %{state | status: status}, :hibernate}
    end
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
    # RoundCommit.stop()
  end

  @spec block_pre_verificacion(block :: map(), reference, :ets.tid()) :: :ok | :error
  def block_pre_verificacion(
        %{
          "creator" => creator_id,
          "height" => height,
          "hash" => hash,
          "signature" => signature,
          "prev" => prev,
          "hashfile" => hashfile,
          "timestamp" => timestamp
        },
        db_ref,
        ets_players
      ) do
    with [{_, player}] <- :ets.lookup(ets_players, creator_id),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         true <- hash == Block.compute_hash(creator_id, height, prev, hashfile, timestamp),
         true <- height == 1 + Sqlite.one("last_block_height_created", [creator_id]) do
      :ok
    else
      _ -> :error
    end
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

  defp spawn_build_foreign_round(
         %{
           block_id: block_id,
           round_id: round_id,
           round_hash: prev_hash,
           db_ref: db_ref,
           balance: balance,
           rcid: rcid,
           miner_pool: pool_pid,
           votes: ets_votes,
           vid: vid,
           tRef: tRef
         },
         message \\ nil
       ) do
    :timer.cancel(tRef)
    pid = self()

    if rcid != vid do
      IO.puts("RM: spawn_build_foreign_round #{round_id}")

      spawn_link(fn ->
        msg_round =
          message || check_votes(%{round_id: round_id, votes: ets_votes})

        IO.inspect(msg_round)

        if msg_round do
          creator = Validator.get(rcid)

          build_round(
            %{
              id: round_id,
              blocks: msg_round.blocks,
              prev: prev_hash,
              signature: msg_round.signature
            },
            block_id,
            creator,
            db_ref,
            balance,
            pool_pid,
            pid
          )
        else
          GenServer.cast(
            pid,
            {:incomplete, Round.cancel(round_id, prev_hash, prev_hash, nil, rcid, 1)}
          )
        end
      end)
    end
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
         vid: vid
       }) do
    pid = self()

    if rcid == vid do
      spawn_link(fn ->
        IO.puts("RM: build_local_round #{round_id}")

        blocks =
          case status do
            :synced ->
              BlockTimer.get_block(block_id) ++
                :ets.tab2list(ets_candidates)

            # Time to wait messages (msg_block) to arrived
            _ ->
              []
          end

        creator = :persistent_term.get(:validator)
        {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)
        hash = Round.compute_hash(round_id, prev_hash, creator.id, hashes)
        {:ok, signature} = Cafezinho.Impl.sign(hash, :persistent_term.get(:privkey))

        # pre-build
        pre_round = %{
          id: round_id,
          blocks: blocks,
          creator: creator.id,
          hash: hash,
          signature: signature,
          prev: prev_hash
        }

        # send message
        NetworkNodes.broadcast(%{"event" => "msg_round", "data" => pre_round})

        build_round(
          %{
            id: round_id,
            hash: hash,
            prev: prev_hash,
            blocks: blocks,
            signature: signature,
            size: size,
            tx_count: tx_count
          },
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

  # creator,
  # balance_pid,
  # balance_tx,
  # tx_count,
  # txs_rejected,
  # size,
  # {supply_tx, supply_key, supply, max_supply}
  defmacrop run_reward do
    quote location: :keep do
      reward = Round.calc_reward(var!(tx_count), var!(txs_rejected), var!(size))
      total = TokenSupply.get(var!(supply)) + reward

      case reward > 0 and var!(max_supply) >= total do
        true ->
          # Update balance
          BalanceStore.income(
            var!(balance_pid),
            var!(balance_tx),
            var!(creator).owner,
            @token,
            reward
          )

          # Update Token Supply
          TokenSupply.add(var!(supply), reward)
          reward

        false ->
          0
      end
    end
  end

  @spec build_round(map, pos_integer, map, reference(), pid, pid, pid) ::
          {:ok, term} | :error
  def build_round(
        %{
          id: round_id,
          blocks: blocks,
          prev: prev_hash,
          signature: signature
        } = map,
        block_id,
        creator,
        db_ref,
        balance_pid,
        pool_pid,
        pid
      ) do
    if Round.null?(map) do
      GenServer.cast(pid, {:incomplete, map})
    else
      creator_id = creator.id
      block_count = length(blocks)

      {hash, tx_count, size} =
        if Map.get(map, :hash) do
          {map.hash, map.tx_count, map.size}
        else
          {hashes, tx_count, size} = Block.hashes_and_count_txs_and_size(blocks)
          hash = Round.compute_hash(round_id, prev_hash, creator.id, hashes)
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
                MinerWorker.mine(worker, Map.put(block, :id, id), creator, round_id)
              end,
              :infinity
            )
          end)
        end)
        |> Task.await_many(:infinity)

      IO.puts("MinerWorker: " <> inspect(result))

      # Count Blocks and txs rejected
      {blocks_approved, txs_rejected} =
        Enum.reduce(result, {[], 0}, fn x, {acc, acc_txr} ->
          case x do
            {:ok, block} -> {acc ++ [block], acc_txr + block.rejected}
            :error -> {acc, acc_txr}
          end
        end)

      block_approved_count = length(blocks_approved)

      if block_approved_count > 0 or block_count == block_approved_count do
        balance_tx = DetsPlux.tx(:balance)
        # Run deferred txs
        TxHandler.run_deferred_txs()

        # Get info native token and current supply
        %{max_supply: max_supply} = Token.get(@token)

        supply = TokenSupply.new(@token)

        # Calculate reward
        reward = run_reward()

        # Run jackpot and events
        jackpot_result =
          {_jackpot_winner, jackpot_amount} =
          run_jackpot(
            db_ref,
            balance_pid,
            balance_tx,
            round_id,
            prev_hash,
            block_id + block_count,
            reward,
            supply,
            max_supply
          )

        # save round
        round = %{
          id: round_id,
          creator: creator_id,
          hash: hash,
          prev: prev_hash,
          signature: signature,
          coinbase: reward + jackpot_amount,
          count: block_count,
          tx_count: tx_count,
          size: size,
          status: 0,
          blocks: blocks_approved,
          extra: nil,
          # extra data
          reward: reward,
          jackpot: jackpot_result
        }

        :done = Round.to_list(round) |> Round.insert()

        run_maintenance(round_id, db_ref)

        GenServer.cast(pid, {:complete, round})

        {:ok, round}
      else
        GenServer.cast(
          pid,
          {:incomplete, Round.cancel(round_id, hash, prev_hash, signature, creator_id, 3)}
        )

        :error
      end
    end
  end

  defp run_jackpot(_, _, _, _, _, _, 0, _, _), do: {nil, 0}
  defp run_jackpot(_, _, _, _, nil, _, _, _, _), do: {nil, 0}

  defp run_jackpot(
         db_ref,
         balances,
         balance_tx,
         round_id,
         round_hash,
         total_blocks,
         reward,
         supply,
         max_supply
       ) do
    if rem(round_id, 100) == 0 do
      IO.inspect("jackpot")
      n = BigNumber.to_int(round_hash)
      dv = min(total_blocks + 1, 20_000)
      b = rem(n, dv) + if(total_blocks >= 20_000, do: total_blocks, else: 0)

      new_amount = TokenSupply.get(supply) + reward

      if max_supply >= new_amount do
        case Block.get(b) do
          nil ->
            {nil, 0}

          block_list ->
            block = Block.list_to_map(block_list)
            tx_count = block.count
            IO.inspect(tx_count)

            cond do
              tx_count > 0 ->
                tx_n = rem(n, tx_count)
                path = Block.decode_path(block.creator, block.height)
                {:ok, content} = File.read(path)
                %{"data" => data} = decode_file!(content)

                winner_id =
                  case Enum.at(data, tx_n) do
                    [_hash, _type, account_id, _nonce, _args, _size] ->
                      BalanceStore.income(balances, balance_tx, account_id, @token, reward)
                      # Update Token Supply
                      TokenSupply.add(supply, reward)

                      account_id

                    [_hash, _type, _arg_key, account_id, _nonce, _args, _size] ->
                      BalanceStore.income(balances, balance_tx, account_id, @token, reward)
                      # Update Token Supply
                      TokenSupply.add(supply, reward)
                      account_id
                  end

                jackpot = [round_id, winner_id, reward]

                :done =
                  Sqlite.step("insert_jackpot", jackpot)

                {winner_id, reward}

              true ->
                {nil, 0}
            end
        end
      else
        {nil, 0}
      end
    else
      {nil, 0}
    end
  end

  defp run_maintenance(0, _), do: nil

  defp run_maintenance(round_id, db_ref) do
    if rem(round_id, 25_000) == 0 do
      Sqlite.step("expiry_refund", [round_id])
      Sqlite.step("expiry_domain", [round_id])
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
    take = min(@max_peers_conn - NetworkNodes.count(), total_players - 1)
    players_connected = NetworkNodes.list()

    if take > 0 do
      :ets.tab2list(ets_players)
      |> Enum.filter(fn {id, _} = x -> id != vid and x not in players_connected end)
      |> Enum.take_random(take)
      |> Enum.map(fn {_id, node} ->
        Task.async(fn ->
          NetworkNodes.connect(node)
        end)
      end)
      |> Task.await_many(:infinity)
      |> Enum.reduce_while(0, fn
        true, acc ->
          {:cont, acc + 1}

        _false, acc ->
          {:cont, acc}
      end)
    else
      0
    end
  end

  # connect to round creator, send candidate if exists and question round creator about msg_round
  defp sync_to_round_creator(
         %{
           rcid: node_id,
           rc_node: node,
           vid: vid,
           round_id: round_id
         } = state
       ) do
    if vid != node_id do
      case check_votes(state) do
        nil ->
          IO.puts("sync_to_round_creator. no votes")
          # connect to round creator
          case NetworkNodes.connect(node) do
            true ->
              candidate = BlockTimer.get_block()

              if candidate do
                NetworkNodes.cast(node, "msg_block", candidate)
              end

              case NetworkNodes.call(node, "get_round", round_id) do
                {:ok, response} when is_map(response) ->
                  send(RoundManager, {"msg_round", Round.from_remote(response), node_id})

                  # Disconnect if count is mayor than to max_peers_conn
                  if NetworkNodes.count() > @max_peers_conn do
                    NetworkNodes.disconnect(node_id)
                  end

                _ ->
                  :ok
              end

            false ->
              Logger.warning("It was not possible to connect to the round creator")
          end

        message ->
          IO.puts("sync_to_round_creator #{inspect(message)}")
          spawn_build_foreign_round(state, message)
      end
    end
  end

  defp check_votes(%{round_id: round_id, votes: ets_votes}) do
    # check votes
    n = NetworkNodes.count()

    :ets.select(ets_votes, [{{{:"$1", :"$2"}, :_, :_}, [{:==, :"$1", round_id}], [:"$_"]}])
    |> Enum.sort(fn {_, _, a}, {_, _, b} -> a >= b end)
    |> List.first()
    |> case do
      nil ->
        nil

      {_, x, count} ->
        if count == div(n, 2) + 1 do
          x
        end
    end
  end
end
