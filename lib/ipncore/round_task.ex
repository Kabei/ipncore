defmodule RoundTask do
  use GenServer, restart: :transient
  require Logger
  alias Ippan.{NetworkNodes, Round}

  @app Mix.Project.config()[:app]
  @max_peers_conn Application.compile_env(@app, :max_peers_conn)

  def start_link(args \\ nil) do
    GenServer.start_link(__MODULE__, args, [])
  end

  @impl true
  def init(_args) do
    {:ok, %{}}
  end

  def get_state_after_check(params) do
    {:ok, pid} = start_link()
    :gen_server.call(pid, {:get_state_after_check, params}, :infinity)
  end

  def sync_to_round_creator(params) do
    {:ok, pid} = start_link()
    :gen_server.call(pid, {:sync_to_round_creator, params}, :infinity)
  end

  @impl true
  def handle_call({:get_state_after_check, params}, _from, state) do
    {:stop, :normal, get_state_after_checkp(params), state}
  end

  def handle_call({:sync_to_round_creator, params}, _from, state) do
    {:stop, :normal, sync_to_round_creatorp(params), state}
  end

  # Check turn of the round and connect to round creator, check another connections
  defp get_state_after_checkp(
         %{players: ets_players, round_id: round_id, total: total_players, vid: vid} = state
       ) do
    position = get_position(round_id, total_players)
    {rcid, rc_node} = get_round_creator(ets_players, position)
    turn = rcid == vid

    IO.puts("RCID: #{rcid} | Position: #{position} | | MyTurn: #{turn}")

    new_state = %{state | position: position, rcid: rcid, rc_node: rc_node, turn: turn}

    connect_to_peers(ets_players, vid, total_players)

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

  defp connect_to_peers(ets_players, vid, total_players) do
    take = min(@max_peers_conn - NetworkNodes.count(), total_players - 1)
    players_connected = NetworkNodes.list()

    if take > 0 do
      :ets.tab2list(ets_players)
      |> Enum.filter(fn {id, _} = x -> id != vid and x not in players_connected end)
      |> Enum.take_random(take)
      |> Enum.map(fn {_id, node} ->
        Task.async(fn -> NetworkNodes.connect(node) end)
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

  defp sync_to_round_creatorp(%{
         rcid: node_id,
         rc_node: validator_node,
         vid: vid,
         round_id: round_id
       }) do
    if vid != node_id do
      # connect to round creator
      case NetworkNodes.connect(validator_node, retry: 2) do
        false ->
          Logger.warning("It was not possible to connect to the round creator")
          :error

        true ->
          case NetworkNodes.call(node_id, "get_round", round_id) do
            {:ok, response} when is_map(response) ->
              Logger.debug("From get_round")

              # Disconnect if count is greater than max_peers_conn
              if NetworkNodes.count() > @max_peers_conn do
                node = NetworkNodes.info(node_id)
                NetworkNodes.disconnect(node)
              end

              {:ok, Round.from_remote(response), node_id}

            {:ok, nil} ->
              :error

            o ->
              Logger.warning("get_round message is not a map")
              IO.inspect(o)
              :error
          end
      end
    end
  end
end
