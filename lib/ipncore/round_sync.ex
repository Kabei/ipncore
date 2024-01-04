defmodule RoundSync do
  use GenServer, restart: :trasient
  require Logger
  alias Ippan.{Round, Validator, NetworkNodes}
  require Validator
  require Sqlite

  #  @type t :: %__MODULE__{status: :synced | :syncing}
  @app Mix.Project.config()[:app]
  @json Application.compile_env(@app, :json)

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
  def handle_continue(
        :prepare,
        %{
          id: current_round_id,
          pid: round_manager_pid
        } = state
      ) do
    hostname = get_random_host()

    case hostname do
      nil ->
        Logger.warning("There is not hosts in whitelist")
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
          stop(state, false)

        {:ok, msg_round} ->
          IO.inspect(msg_round)
          round = Round.from_remote(msg_round)
          %{block_id: last_block_id} = :sys.get_state(RoundManager)
          IO.inspect(round)
          creator = Validator.get(round.creator)

          case RoundManager.build_round(
                 round,
                 last_block_id,
                 creator,
                 db_ref,
                 balances,
                 miner_pool_pid,
                 round_manager_pid
               ) do
            {:ok, _new_round} ->
              {:noreply, state, {:continue, {:fetch, round_id + 1, node}}}

            _error ->
              stop(state, false)
          end
      end
    else
      {
        :noreply,
        Map.delete(state, :last),
        {:continue, {:after, :ets.first(state.queue)}}
      }
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
      [{_id, round}] ->
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

  # Add round in a queue
  @impl true
  def handle_cast({:add, %{id: id} = new_round}, state = %{queue: ets_queue}) do
    :ets.insert(ets_queue, {id, new_round})
    {:noreply, state}
  end

  # Get random hostname from whitelist
  defp get_random_host do
    File.stream!("whitelist", [], :line)
    |> Enum.map(fn text ->
      String.trim(text)
    end)
    |> Enum.filter(fn x -> Match.hostname?(x) or Match.ipv4(x) end)
    |> Enum.take_random(1)
    |> List.first()
  end

  defp check(hostname, my_last_round) do
    {:ok, r1} = HTTPoison.get("https://#{hostname}/v1/info", [], hackney: [:insecure])
    {:ok, r2} = HTTPoison.get("https://#{hostname}/v1/round/last", [], hackney: [:insecure])

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
  end

  defp stop(state = %{queue: ets_queue, pid: round_manager_pid}, next) do
    :ets.delete(ets_queue)
    GenServer.cast(round_manager_pid, {:status, :synced, next})
    {:stop, :normal, state}
  end
end
