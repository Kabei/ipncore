defmodule RoundSync do
  use GenServer, restart: :trasient
  require Ippan.Round
  alias Ippan.{ClusterNodes, Round, Validator, NetworkNodes}
  require Logger
  require Round
  require Validator
  require Sqlite

  @ets_name :queue
  @ets_opts [
    :ordered_set,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]

  @app Mix.Project.config()[:app]
  @json Application.compile_env(@app, :json)
  @blockchain Application.compile_env(@app, :name)
  @offset 200

  def start_link(args) do
    case Process.whereis(__MODULE__) do
      nil ->
        GenServer.start_link(__MODULE__, args, name: __MODULE__)

      pid ->
        {:already_stated, pid}
    end
  end

  @impl true
  def init(
        state = %{
          block_id: _block_id,
          db_ref: _db_ref,
          balance: _balances,
          miner_pool: _miner_pool_pid,
          pid: _pid
        }
      ) do
    ets_queue = :ets.new(@ets_name, @ets_opts)

    {:ok, Map.put(state, :queue, ets_queue), {:continue, :prepare}}
  end

  # Check last state from multiples nodes
  @impl true
  def handle_continue(:prepare, state) do
    stats = Stats.new()
    current_round_id = Stats.get(stats, "last_round")
    hosts = get_whitelist()

    case hosts do
      [] ->
        Logger.warning("No hosts in whitelist file")
        stop(state, true)

      hosts ->
        do_results(hosts, current_round_id, state)
    end
  end

  # Get old rounds data from node selected
  def handle_continue(
        {:fetch, round_id, %{id: node_id} = node},
        %{
          offset: offset,
          starts: starts,
          target: target_id,
          balance: balances,
          block_id: last_block_id,
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } =
          state
      ) do
    if round_id <= target_id do
      case NetworkNodes.call(node_id, "get_rounds", %{
             "limit" => @offset,
             "offset" => offset,
             "starts" => starts
           }) do
        {:error, _} ->
          Logger.warning("Roundsync error call starts: ##{round_id}")
          :timer.sleep(1000)
          {:noreply, state, {:continue, {:fetch, round_id, node}}}

        {:ok, nil} ->
          stop(state, true)

        {:ok, []} ->
          stop(state, true)

        {:ok, rounds} ->
          # Build rounds and return total new blocks
          result_block_id =
            Enum.reduce(rounds, last_block_id, fn msg_round, bid ->
              %{id: id} = round = Round.sync_remote(msg_round)
              creator = Validator.get(round.creator)

              unless Round.exists?(id) do
                RoundManager.build_round(
                  round,
                  bid,
                  %{creator | hostname: node.hostname},
                  db_ref,
                  balances,
                  miner_pool_pid,
                  round_manager_pid,
                  false,
                  false
                )

                ClusterNodes.broadcast(%{"event" => "round.new", "data" => round})
              end

              bid + length(round.blocks)
            end)

          len = length(rounds)
          new_state = %{state | block_id: result_block_id, offset: offset + len}

          {:noreply, new_state, {:continue, {:fetch, round_id + len, node}}}
      end
    else
      {
        :noreply,
        Map.drop(state, ~w(offset starts target)a),
        {:continue, {:after, :ets.next(state.queue, round_id), node}}
      }
    end
  end

  def handle_continue({:after, :"$end_of_table", _node}, state) do
    stop(state, true)
  end

  def handle_continue(
        {:after, key, node},
        %{
          queue: ets_queue,
          balance: balances,
          block_id: last_block_id,
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } = state
      ) do
    case :ets.lookup(ets_queue, key) do
      [{_id, round}] ->
        creator = Validator.get(round.creator)

        RoundManager.build_round(
          round,
          last_block_id,
          %{creator | hostname: node.hostname},
          db_ref,
          balances,
          miner_pool_pid,
          round_manager_pid,
          false,
          false
        )

        ClusterNodes.broadcast(%{"event" => "round.new", "data" => round})

        next_key = :ets.next(ets_queue, key)
        :ets.delete(ets_queue, key)
        new_state = %{state | block_id: last_block_id + length(round.blocks)}
        {:noreply, new_state, {:continue, {:after, next_key, node}}}

      _ ->
        stop(state, true)
    end
  end

  # Add round in a queue
  def add_queue(msg_round) do
    :ets.insert(@ets_name, {msg_round.id, msg_round})
  end

  @filename "whitelist"
  # Get hosts from whitelist
  defp get_whitelist do
    if File.exists?(@filename) do
      File.stream!(@filename, [], :line)
      |> Enum.map(fn text ->
        String.trim(text)
      end)
    end || []
  end

  defp check(hostname, my_last_round) do
    try do
      my_last_snap = Snapshot.last()

      case HTTPoison.get("https://#{hostname}/v1/info", [], hackney: [:insecure]) do
        {:ok, r1 = %{status_code: 200}} ->
          case HTTPoison.get("https://#{hostname}/v1/network/status", [],
                 hackney: [:insecure],
                 timeout: 10_000
               ) do
            {:ok, r2 = %{status_code: 200}} ->
              {:ok, validator} = @json.decode(r1.body)

              {:ok, %{"id" => round_id, "name" => blockchain, "snapshot" => last_snap}} =
                @json.decode(r2.body)

              last_snap = Snapshot.to_map(last_snap)

              cond do
                blockchain != @blockchain ->
                  Logger.warning("Wrong blockchain \"#{blockchain}\" - My config: #{@blockchain}")
                  :error

                last_snap.id > my_last_snap.id ->
                  Logger.warning("Snapshot downloading: ##{last_snap.id}")

                  case Snapshot.download(hostname, last_snap) do
                    :ok ->
                      Snapshot.restore(last_snap.id)
                      :idle

                    :error ->
                      Logger.warning("Error snapshot downloading or verification hash")
                      :error
                  end

                round_id > my_last_round ->
                  # validator
                  node =
                    validator
                    |> Map.take(~w(id hostname port pubkey net_pubkey))
                    |> MapUtil.to_atoms()
                    |> MapUtil.transform(:pubkey, fn x -> Base.decode64!(x) end)
                    |> MapUtil.transform(:net_pubkey, fn x -> Base.decode64!(x) end)

                  case NetworkNodes.connect(node) do
                    false ->
                      Logger.warning("Error connecting to P2P #{hostname}")
                      :error

                    _true ->
                      {:ok, round_id, node}
                  end

                true ->
                  Logger.info("No Sync")
                  :idle
              end

            _ ->
              :error
          end

        {:ok, _} ->
          :error

        {:error, err} ->
          Logger.warning("Error connecting to #{hostname} | reason: #{inspect(err.reason)}")

          :error
      end
    rescue
      error ->
        Logger.error(Exception.format(:error, error, __STACKTRACE__))
        :error
    end
  end

  defp do_results([], _, state) do
    IO.puts("RoundSync Stop")
    stop(state, true)
  end

  defp do_results([host | hosts], current_round_id, state) do
    case check(host, current_round_id) do
      {:ok, last_round_id, node} ->
        IO.puts("RoundSync Active")

        map = %{
          offset: 0,
          starts: current_round_id + 1,
          target: last_round_id
        }

        {
          :noreply,
          Map.merge(state, map),
          {:continue, {:fetch, current_round_id + 1, node}}
        }

      :idle ->
        stop(state, true)

      :error ->
        do_results(hosts, current_round_id, state)
    end
  end

  defp stop(state = %{queue: ets_queue, pid: round_manager_pid}, next) do
    :ets.delete(ets_queue)
    GenServer.cast(round_manager_pid, {:status, :synced, next})
    {:stop, :normal, state}
  end
end
