defmodule RoundSync do
  use GenServer, restart: :trasient
  alias Ippan.Validator
  alias Ippan.NetworkNodes
  require Validator
  require Sqlite

  #  @type t :: %__MODULE__{status: :synced | :syncing}

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(%{
        round_id: round_id,
        round_hash: round_hash,
        miner_pool: miner_pool_pid,
        pid: pid
      }) do
    {:ok,
     %{
       round_id: round_id,
       round_hash: round_hash,
       miner_pool: miner_pool_pid,
       pid: pid,
       queue: MapSet.new()
     }, {:continue, :sync}}
  end

  # Check last state from multiples nodes
  @impl true
  def handle_continue(:sync, %{pid: round_manager_pid} = state) do
    list = NetworkNodes.list()

    data =
      Enum.take_random(list, 10)
      |> Enum.map(fn {_node_id, node} ->
        Task.async(fn ->
          case NetworkNodes.connect(node) do
            true ->
              case NetworkNodes.call(node, "get_state") do
                {:ok, res} ->
                  {:ok, Map.put(res, "node_id", node.id)}

                e ->
                  e
              end

            false ->
              :error
          end
        end)
      end)
      |> Task.await_many(:infinity)
      |> Enum.filter(fn
        {:ok, _} -> true
        _ -> false
      end)
      |> Enum.map(fn {_, x} -> x end)
      |> Enum.group_by(fn x -> Map.get(x, "hash") end)
      |> Enum.sort_by(fn {_k, x} -> length(x) end, :desc)

    nodes = Enum.map(data, fn {_hash, x} -> Map.get(x, "node_id") end)

    data
    |> List.first()
    |> case do
      nil ->
        IO.puts("pase 1")
        stop(state, true)

      current_state ->
        IO.puts("pase 2")
        GenServer.cast(round_manager_pid, {:put, current_state})
        GenServer.cast(round_manager_pid, {:state, :syncing, true})
        {:noreply, state, {:continue, {:fetch, current_state, nodes}}}
    end
  end

  # Get old rounds data from node selected
  def handle_continue(
        {:fetch, %{"id" => last_round_id} = _last_state, nodes},
        %{round_id: old_round_id} = state
      ) do
    [node_id] = Enum.take_random(nodes, 1)
    node = NetworkNodes.info(node_id)

    diff = last_round_id - old_round_id

    if diff > 0 do
      rounds =
        1..diff
        |> Enum.reduce(%{}, fn i, acc ->
          case NetworkNodes.call(node, "get_rounds", %{
                 "from_id" => diff,
                 "limit" => 50,
                 "offset" => i * 50
               }) do
            {:ok, rounds} when is_list(rounds) ->
              [rounds | acc]

            _ ->
              acc
          end
        end)
        |> Enum.reverse()

      {:noreply, state, {:continue, {:build, rounds}}}
    else
      stop(state, false)
    end
  end

  # Build old rounds
  def handle_continue({:build, rounds}, old_state) do
    pid = self()

    spawn_link(fn ->
      build(rounds, old_state)
      GenServer.cast(pid, :end)
    end)
  end

  # Add round in a queue
  @impl true
  def handle_cast({:add, new_round}, state = %{queue: queue}) do
    {:noreply, %{state | queue: MapSet.put(queue, new_round)}}
  end

  # Build queue rounds
  def handle_cast(:end, %{queue: queue} = state) do
    build(MapSet.to_list(queue), state)

    stop(state, false)
  end

  # Build a list of rounds
  defp build(rounds, %{miner_pool: miner_pool_pid, pid: round_manager_pid} = old_state) do
    db_ref = :persistent_term.get(:main_conn)
    balances = DetsPlux.get(:balance)

    Enum.reduce(rounds, old_state, fn round, %{block_id: block_id} = acc ->
      creator = Validator.get(round.creator)

      case RoundManager.build_round(
             round,
             block_id,
             creator,
             db_ref,
             balances,
             miner_pool_pid,
             round_manager_pid
           ) do
        {:ok, new_round} ->
          %{
            round_id: new_round.id + 1,
            block_id: block_id + round.count,
            round_hash: new_round.hash
          }

        _ ->
          acc
      end
    end)
  end

  defp stop(state = %{pid: round_manager_pid}, next) do
    GenServer.cast(round_manager_pid, {:status, :synced, next})
    {:stop, :normal, state}
  end
end
