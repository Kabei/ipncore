defmodule RoundSync do
  use GenServer, restart: :trasient
  alias Ippan.{Round, Validator, NetworkNodes}
  require Validator
  require Sqlite

  #  @type t :: %__MODULE__{status: :synced | :syncing}

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(
        state = %{
          id: _round_id,
          block_id: _block_id,
          balance: _balances,
          hash: _round_hash,
          miner_pool: _miner_pool_pid,
          pid: _pid
        }
      ) do
    ets_queue = :ets.new(:queue, [:ordered_set])

    {:ok, Map.put(state, :queue, ets_queue), {:continue, :prepare}}
  end

  # Check last state from multiples nodes
  @impl true
  def handle_continue(:prepare, %{id: current_round_id, pid: round_manager_pid} = state) do
    list = NetworkNodes.list()

    data =
      Enum.take_random(list, 10)
      |> Enum.map(fn {_node_id, node} ->
        Task.async(fn ->
          case NetworkNodes.connect(node) do
            true ->
              case NetworkNodes.call(node, "last_round") do
                {:ok, %{"id" => rid} = res} when rid > current_round_id ->
                  {:ok, Map.put(res, "node", node.id)}

                _e ->
                  :none
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
      |> case do
        [] ->
          nil

        result ->
          result
          |> Enum.map(fn {_, x} -> x end)
          |> Enum.group_by(fn %{"hash" => hash, "id" => id} -> {id, hash} end)
          |> Enum.sort_by(fn {_key, x} -> length(x) end, :desc)
          |> List.first()
          |> Enum.map(fn {_key, x} -> x end)
      end

    data
    |> case do
      nil ->
        IO.puts("pase 1 nil")
        stop(state, true)

      [] ->
        IO.puts("pase 1")
        stop(state, true)

      result ->
        IO.puts("pase 2")

        %{"id" => last_round_id} = status = List.first(result) |> Map.delete("node")
        nodes = Enum.map(result, fn %{"node" => node_id} -> node_id end)
        GenServer.cast(round_manager_pid, {:put, status})
        GenServer.cast(round_manager_pid, {:status, :syncing, true})

        {:noreply, Map.put(state, :last, last_round_id),
         {:continue, {:fetch, current_round_id + 1, nodes}}}
    end
  end

  # Get old rounds data from node selected
  def handle_continue(
        {:fetch, round_id, nodes},
        %{
          last: last_round_id,
          block_id: block_id,
          balance: balances,
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } =
          state
      ) do
    [node_id] = Enum.take_random(nodes, 1)

    if last_round_id > 0 do
      case NetworkNodes.call(node_id, "get_round", round_id) do
        {:error, _} ->
          stop(state, false)

        msg_round ->
          round = Round.from_remote(msg_round)

          case RoundManager.build_round(
                 round,
                 block_id,
                 round.creator,
                 db_ref,
                 balances,
                 miner_pool_pid,
                 round_manager_pid
               ) do
            {:ok, _new_round} ->
              {:noreply, state, {:continue, {:fetch, round_id + 1, nodes}}}

            _ ->
              stop(state, false)
          end
      end
    else
      {:noreply, Map.delete(state, :last), {:continue, {:after, :ets.first(state.queue)}}}
    end
  end

  def handle_continue({:after, :"$end_of_table"}, state) do
    stop(state, true)
  end

  def handle_continue(
        {:after, key},
        %{
          queue: ets_queue,
          block_id: block_id,
          balance: balances,
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } = state
      ) do
    case :ets.lookup(ets_queue, key) do
      [round] ->
        RoundManager.build_round(
          round,
          block_id,
          round.creator,
          db_ref,
          balances,
          miner_pool_pid,
          round_manager_pid
        )

        next_key = :ets.next(ets_queue, key)
        :ets.delete(ets_queue, key)
        {:noreply, state, {:continue, {:after, next_key}}}

      _ ->
        stop(state, true)
    end
  end

  # Build old rounds
  # def handle_continue({:build, rounds}, old_state) do
  #   pid = self()

  #   spawn_link(fn ->
  #     build(rounds, old_state)
  #     GenServer.cast(pid, :end)
  #   end)

  #   {:noreply, old_state}
  # end

  # Add round in a queue
  @impl true
  def handle_cast({:add, %{id: id} = new_round}, state = %{queue: ets_queue}) do
    :ets.insert(ets_queue, {id, new_round})
    {:noreply, state}
  end

  # Build queue rounds
  # def handle_cast(:end, %{queue: ets_queue} = state) do
  #   :ets.delete(ets_queue)

  #   stop(state, false)
  # end

  # Build a list of rounds
  # defp build(rounds, %{miner_pool: miner_pool_pid, pid: round_manager_pid} = old_state) do
  #   db_ref = :persistent_term.get(:main_conn)
  #   balances = DetsPlux.get(:balance)

  #   Enum.reduce(rounds, old_state, fn round, %{block_id: block_id} = acc ->
  #     creator = Validator.get(round.creator)

  #     case RoundManager.build_round(
  #            round,
  #            block_id,
  #            creator,
  #            db_ref,
  #            balances,
  #            miner_pool_pid,
  #            round_manager_pid
  #          ) do
  #       {:ok, new_round} ->
  #         %{
  #           round_id: new_round.id + 1,
  #           block_id: block_id + round.count,
  #           round_hash: new_round.hash
  #         }

  #       _ ->
  #         acc
  #     end
  #   end)
  # end

  defp stop(state = %{queue: ets_queue, pid: round_manager_pid}, next) do
    :ets.delete(ets_queue)
    GenServer.cast(round_manager_pid, {:status, :synced, next})
    {:stop, :normal, state}
  end
end
