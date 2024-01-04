defmodule RoundSync do
  use GenServer, restart: :trasient
  require Ippan.Round
  alias Ippan.{Round, Validator, NetworkNodes}
  require Logger
  require Round
  require Validator
  require Sqlite

  #  @type t :: %__MODULE__{status: :synced | :syncing}
  @app Mix.Project.config()[:app]
  @json Application.compile_env(@app, :json)

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
          db_ref: _db_ref,
          balance: _balances,
          miner_pool: _miner_pool_pid,
          pid: _pid
        }
      ) do
    ets_queue = :ets.new(:queue, [:ordered_set])

    {:ok, Map.put(state, :queue, ets_queue), {:continue, :prepare}}
  end

  # Check last state from multiples nodes
  @impl true
  def handle_continue(
        :prepare,
        %{
          db_ref: db_ref,
          pid: round_manager_pid
        } = state
      ) do
    {current_round_id, _hash} = Round.last()
    hostname = get_random_host()

    case hostname do
      nil ->
        Logger.warning("There are no hosts in the whitelist file")
        stop(state, true)

      hostname ->
        case check(hostname, current_round_id) do
          {:ok, last_round_id, node} ->
            IO.puts("RoundSync Active")
            # GenServer.cast(round_manager_pid, {:put, status})
            GenServer.cast(round_manager_pid, {:status, :syncing, true})

            {
              :noreply,
              Map.put(state, :last, last_round_id),
              {:continue, {:fetch, current_round_id + 1, node}}
            }

          :stop ->
            IO.puts("RoundSync Stop")
            stop(state, false)

          :idle ->
            IO.puts("RoundSync Idle")
            stop(state, true)
        end
    end
  end

  # Get old rounds data from node selected
  def handle_continue(
        {:fetch, round_id, %{id: node_id} = node},
        %{
          last: last_round_id,
          balance: balances,
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } =
          state
      ) do
    if last_round_id != round_id do
      case NetworkNodes.call(node_id, "get_round", round_id) do
        {:error, _} ->
          Logger.warning("Roundsync error call #{round_id}")
          :timer.sleep(1000)
          {:noreply, state, {:continue, {:fetch, round_id, node}}}

        {:ok, nil} ->
          stop(state, true)

        {:ok, msg_round} ->
          round = Round.from_remote(msg_round)
          last_block_id = GenServer.call(round_manager_pid, :last_block)
          creator = Validator.get(round.creator)

          case RoundManager.build_round(
                 round,
                 last_block_id,
                 %{creator | hostname: node.hostname},
                 db_ref,
                 balances,
                 miner_pool_pid,
                 round_manager_pid,
                 false
               ) do
            {:ok, _new_round} ->
              {:noreply, state, {:continue, {:fetch, round_id + 1, node}}}

            _error ->
              {:noreply, state, {:continue, {:fetch, round_id + 1, node}}}
          end
      end
    else
      {
        :noreply,
        Map.delete(state, :last),
        {:continue, {:after, :ets.first(state.queue), node}}
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
          db_ref: db_ref,
          miner_pool: miner_pool_pid,
          pid: round_manager_pid
        } = state
      ) do
    case :ets.lookup(ets_queue, key) do
      [{_id, round}] ->
        last_block_id = GenServer.call(round_manager_pid, :last_block)
        creator = Validator.get(round.creator)

        RoundManager.build_round(
          round,
          last_block_id,
          %{creator | hostname: node.hostname},
          db_ref,
          balances,
          miner_pool_pid,
          round_manager_pid,
          false
        )

        next_key = :ets.next(ets_queue, key)
        :ets.delete(ets_queue, key)
        {:noreply, state, {:continue, {:after, next_key, node}}}

      _ ->
        stop(state, true)
    end
  end

  # Add round in a queue
  @impl true
  def handle_cast({:add, %{id: id} = new_round}, state = %{queue: ets_queue}) do
    :ets.insert(ets_queue, {id, new_round})
    {:noreply, state}
  end

  @filename "whitelist"
  # Get random hostname from whitelist
  defp get_random_host do
    if File.exists?(@filename) do
      File.stream!(@filename, [], :line)
      |> Enum.map(fn text ->
        String.trim(text)
      end)
      |> Enum.filter(fn x -> Match.hostname?(x) or Match.ipv4(x) end)
      |> Enum.take_random(1)
      |> List.first()
    end
  end

  defp check(hostname, my_last_round) do
    try do
      {:ok, r1 = %{status_code: 200}} =
        HTTPoison.get("https://#{hostname}/v1/info", [], hackney: [:insecure])

      case HTTPoison.get("https://#{hostname}/v1/round/last", [], hackney: [:insecure]) do
        {:ok, r2 = %{status_code: 200}} ->
          {:ok, validator} = @json.decode(r1.body)

          {:ok, %{"id" => round_id}} = @json.decode(r2.body)

          if String.to_integer(round_id) > my_last_round do
            # validator
            node =
              validator
              |> Map.take(~w(id hostname port pubkey net_pubkey))
              |> MapUtil.to_atoms()
              |> MapUtil.transform(:pubkey, fn x -> Base.decode64!(x) end)
              |> MapUtil.transform(:net_pubkey, fn x -> Base.decode64!(x) end)

            case NetworkNodes.connect(node) do
              false ->
                Logger.warning("It is not possible connect to #{hostname}")
                :stop

              _socket ->
                {:ok, round_id, node}
            end
          else
            Logger.info("No Sync")
            :idle
          end

        _ ->
          :idle
      end
    rescue
      error ->
        Logger.error(Exception.format(:error, error, __STACKTRACE__))
        :idle
    end
  end

  defp stop(state = %{queue: ets_queue, pid: round_manager_pid}, next) do
    :ets.delete(ets_queue)
    GenServer.cast(round_manager_pid, {:status, :synced, next})
    {:stop, :normal, state}
  end
end
