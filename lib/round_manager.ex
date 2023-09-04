defmodule RoundManager do
  use GenServer, restart: :transient
  require Logger
  alias Ippan.{NetworkNode, Validator, MsgBlock, MsgRound}
  alias Phoenix.PubSub
  require SqliteStore

  @pubsub :network
  @timeout 10_000
  @max_peers_conn Application.compile_env(:ipncore, :max_peers_conn)

  @impl true
  def init(%{round: round}) do
    vid = :persistent_term.get(:vid)
    main = {:persistent_term.get(:asset_conn), :persistent_term.get(:asset_stmt)}
    net = {:persistent_term.get(:net_conn), :persistent_term.get(:net_stmt)}

    ets_players =
      :ets.new(:players, [:set, :public, read_concurrency: true, write_concurrency: true])

    ets_votes =
      :ets.new(:votes, [:set, :public, read_concurrency: true, write_concurrency: true])

    {:ok,
     %{
       id: vid,
       main: main,
       net: net,
       players: ets_players,
       ets_votes: ets_votes,
       round: round,
       tRef: nil,
       delegate: false,
       total: 0
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(
        :next,
        state = %{
          id: vid,
          main: {conn, stmts},
          players: ets_players,
          round: round
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

    delegates = get_delegates(round.id, total_players)

    tRef = :timer.send_after(@timeout, :tick)

    {:noreply,
     %{
       state
       | delegates: delegates,
         delegate: vid in delegates,
         tRef: tRef,
         total: total_players,
         vid: vid
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

  def handle_info({:start, round}, %{total: total_players, vid: vid} = state) do
    delegates = get_delegates(round.id, total_players)
    delegate = vid in delegates
    {:noreply, %{state | delegates: delegates, delegate: delegate}}
  end

  def handle_info(
        {
          "msg_round",
          _node_id,
          msg_round = %{
            "id" => id,
            "creator" => creator,
            "messages" => _messages,
            "hash" => hash,
            "signature" => signature
          }
        },
        %{
          net: {net_conn, net_stmts},
          players: ets_players,
          ets_votes: ets_votes,
          min: min_votes,
          delegates: delegates,
          round: %{id: round_id}
        } =
          state
      )
      when id > round_id do
    with true <- creator in delegates,
         [{_, player}] <- :ets.lookup(ets_players, creator),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         count <-
           :ets.update_counter(ets_votes, id, {3, 1}, {id, msg_round, 1}) do
      PubSub.local_broadcast(@pubsub, "network", msg_round)

      if count == min_votes do
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
      else
        {:noreply, state}
      end
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

  @impl true
  def terminate(_reason, _state) do
    :ok
  end

  defp connect_to_peers(players) do
    take = max(@max_peers_conn - NetworkNode.count(), 0)

    if take > 0 do
      (players -- NetworkNode.list())
      |> Enum.take_random(take)
      |> Enum.each(fn x ->
        NetworkNode.connect(x, retry: 3)
      end)
    end
  end

  defp get_delegates(round, total_players) do
    count = EnvStore.round_delegates()
    start = rem(round * count, total_players)

    position_end = start + total_players

    if position_end <= total_players do
      start..total_players |> Enum.to_list()
    else
      rest = position_end - total_players
      Enum.to_list(start..position_end) ++ Enum.to_list(0..rest)
    end
  end
end
