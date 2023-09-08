defmodule RoundManager do
  use GenServer, restart: :transient
  require Logger
  alias Ippan.{NetworkNode, Validator, MsgBlock, MsgRound}
  alias Phoenix.PubSub
  require SqliteStore

  @pubsub :cluster
  @timeout 10_000
  # @block_interval Application.compile_env(@otp_app, :block_interval)
  @max_peers_conn Application.compile_env(:ipncore, :max_peers_conn)

  @impl true
  def init(_args) do
    vid = :persistent_term.get(:vid)
    main = {conn = :persistent_term.get(:asset_conn), stmts = :persistent_term.get(:asset_stmt)}
    net = {:persistent_term.get(:net_conn), :persistent_term.get(:net_stmt)}

    ets_players =
      :ets.new(:players, [:ordered_set, :public, read_concurrency: true, write_concurrency: true])

    ets_votes =
      :ets.new(:votes, [:set, :public, read_concurrency: true, write_concurrency: true])

    [round_id, round_hash] =
      SqliteStore.fetch(conn, stmts, "last_round", [], [0, nil])

    [block_id, block_hash] =
      SqliteStore.fetch(conn, stmts, "last_block", [vid], [0, nil])

    PubSub.subscribe(@pubsub, "validator")
    PubSub.subscribe(@pubsub, "env")

    {:ok,
     %{
       vid: vid,
       main: main,
       net: net,
       players: ets_players,
       ets_votes: ets_votes,
       round_id: round_id,
       round_hash: round_hash,
       block_id: block_id,
       block_hash: block_hash,
       tRef: nil,
       delegate: false,
       total_delegates: EnvStore.round_delegates(),
       total: 0
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(
        :next,
        state = %{
          vid: vid,
          main: {conn, stmts},
          players: ets_players,
          round_id: round_id,
          total_delegates: total_delegates
        }
      ) do
    players = SqliteStore.all(conn, stmts, "get_players")
    total_players = length(players)

    # fill nodes table
    for v <- players do
      :ets.insert(ets_players, Validator.to_tuple(v))
    end

    # connect to P2P peers
    connect_to_peers(players)

    # check round positions
    list_positions = get_list_delegates(round_id, total_players, total_delegates)
    delegates_vid = get_vid_delegates(ets_players, list_positions)
    position = get_position(round_id, total_players)
    turn_vid = has_turn(ets_players, position)
    delegate = vid in delegates_vid
    my_turn = turn_vid == vid

    tRef = :timer.send_after(@timeout, :tick)

    {:noreply,
     %{
       state
       | delegates: delegates_vid,
         delegate: delegate,
         my_turn: my_turn,
         tRef: tRef,
         total: total_players
     }}
  end

  @impl true
  def handle_info(:tick, state) do
    # send vote failure

    {:noreply, state}
  end

  def handle_info({:complete_sync, _round}, state) do
    # send vote failure

    {:noreply, state}
  end

  # def handle_info({:start, round}, %{total: total_players, vid: vid} = state) do
  #   delegates = get_delegates(round.id, total_players)
  #   delegate = vid in delegates
  #   {:noreply, %{state | delegates: delegates, delegate: delegate}}
  # end

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
          delegates: delegates,
          round_id: round_id
        } =
          state
      )
      when id > round_id do
    #  count <-
    #    :ets.update_counter(ets_votes, id, {3, 1}, {id, msg_round, 1})
    with true <- creator in delegates,
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
        %{
          players: ets_players,
          vid: vid,
          total: total_players,
          round_id: round_id,
          total_delegates: total_delegates
        } = state
      ) do
    :ets.insert(ets_players, Validator.to_tuple(validator))

    # check round positions
    list_positions = get_list_delegates(round_id, total_players, total_delegates)
    delegates_vid = get_vid_delegates(ets_players, list_positions)
    position = get_position(round_id, total_players)
    turn_vid = has_turn(ets_players, position)
    delegate = vid in delegates_vid
    my_turn = turn_vid == vid

    {:noreply,
     %{
       state
       | total: total_players + 1,
         delegates: delegates_vid,
         delegate: delegate,
         my_turn: my_turn
     }}
  end

  def handle_info(
        %{"event" => "validator.delete", "data" => validator_id},
        %{players: ets_players, total: total_players} = state
      ) do
    :ets.delete(ets_players, validator_id)
    {:noreply, %{state | total: total_players - 1}}
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub, "validator")
    :ok
  end

  defp connect_to_peers(players) do
    take = max(@max_peers_conn - NetworkNode.count(), 0)

    if take > 0 do
      (players -- NetworkNode.list())
      |> Enum.take_random(take)
      |> Enum.each(fn x ->
        NetworkNode.connect_async(x)
      end)
    end
  end

  defp get_list_delegates(round, total_players, total_delegates) do
    p = total_players - 1
    start_pos = div(rem(round, total_players), total_delegates) * total_delegates
    end_pos = start_pos + total_delegates - 1
    rest = end_pos - p

    cond do
      rest > 0 ->
        Enum.to_list(start_pos..p) ++ Enum.to_list(0..(rest - 1))

      true ->
        Enum.to_list(start_pos..end_pos)
    end
  end

  def get_vid_delegates(ets_players, range) do
    for i <- range do
      [{key, _}] = :ets.slot(ets_players, i)
      key
    end
  end

  def get_position(round, total_players) do
    rem(round, total_players)
  end

  def has_turn(ets_players, position) do
    :ets.slot(ets_players, position)
  end

  def total_players(ets_players) do
    :ets.info(ets_players, :size)
  end
end
