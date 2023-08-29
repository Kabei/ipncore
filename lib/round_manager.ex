defmodule RoundManager do
  alias Ippan.NetworkNode
  alias Ippan.Validator
  alias Ippan.MsgBlock
  alias Ippan.MsgRound
  alias Phoenix.PubSub
  use GenServer, restart: :transient
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

    {:ok,
     %{
       id: vid,
       main: main,
       net: net,
       players: ets_players,
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

    delegate = is_delegate?(round, total_players, vid)

    tRef = :timer.send_after(@timeout, :tick)
    {:noreply, %{state | delegate: delegate, tRef: tRef, total: total_players}}
  end

  @impl true
  def handle_info(:tick, state) do
    # send vote failure

    {:noreply, state}
  end

  def handle_info({:round_start, _round}, state) do
    # send vote failure

    {:noreply, state}
  end

  def handle_info(
        %{
          "event" => "msg_round",
          "data" =>
            msg_round = %{
              "id" => _id,
              "creator" => creator,
              "messages" => _messages,
              "hash" => hash,
              "signature" => signature
            }
        },
        %{net: {net_conn, net_stmts}, players: ets_players} = state
      ) do
    with [{_, player}] <- :ets.lookup(ets_players, creator),
         :ok <- Cafezinho.Impl.verify(signature, hash, player.pubkey),
         :done <-
           SqliteStore.step(net_conn, net_stmts, "insert_msg_round", MsgRound.encode(msg_round)) do
      PubSub.local_broadcast(@pubsub, "network", msg_round)
    end

    {:noreply, state}
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

  defp is_delegate?(round, total_players, my_id) do
    rem(round, total_players) == my_id
  end
end
