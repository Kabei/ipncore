defmodule BlockTimer do
  use GenServer
  alias Ippan.{Block, BlockHandler}
  require Block
  require Sqlite

  @app Mix.Project.config()[:app]
  @module __MODULE__
  # @timeout Application.compile_env(@app, :block_interval)
  @timeout 15_000
  @message :mine
  @min_time 750
  @max_time 5_000

  def start_link(args) do
    case Process.whereis(@module) do
      nil ->
        GenServer.start_link(@module, args, name: @module)

      _pid ->
        stop()
        GenServer.start_link(@module, args, name: @module)
    end
  end

  @impl true
  def init(%{db_ref: db_ref, block_id: block_id, creator: creator_id}) do
    [last_height, prev] =
      Block.last_created(creator_id, [-1, nil])

    {:ok,
     %{
       block_id: block_id,
       creator: creator_id,
       height: last_height + 1,
       prev: prev,
       candidate: nil,
       tRef: nil
     }, {:continue, :next}}
  end

  @impl true
  def handle_continue(:next, %{candidate: candidate, tRef: tRef} = state) do
    case candidate do
      nil ->
        :timer.cancel(tRef)
        {:ok, tRef} = :timer.send_after(@timeout, @message)

        {:noreply, %{state | tRef: tRef}, :hibernate}

      _ ->
        {:noreply, state, :hibernate}
    end
  end

  @doc """
  Get a candidate
  """
  @spec get_block :: map() | nil
  def get_block do
    GenServer.call(@module, :get, :infinity)
  end

  @doc """
  Get a candidate with a dynamic time to wait
  """
  @spec get_blocks(block_id :: integer()) :: [map()] | []
  def get_blocks(block_id) do
    case GenServer.call(@module, {:get, block_id}, :infinity) do
      nil -> []
      candidate -> [candidate]
    end
  end

  @doc """
  Update block height, prev hash and candidate in state
  """
  @spec complete(db_ref :: reference()) :: :ok
  def complete(db_ref) do
    GenServer.cast(@module, {:complete, db_ref})
  end

  @spec stop :: :ok
  def stop do
    GenServer.stop(@module, :normal, :infinity)
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state.candidate, state}
  end

  def handle_call({:get, current_block_id}, _from, %{block_id: block_id, tRef: tRef} = state) do
    :timer.cancel(tRef)

    diff = current_block_id - block_id

    cond do
      diff > 5 ->
        :timer.sleep(@min_time)

      diff <= 1 ->
        :timer.sleep(@max_time)

      true ->
        :timer.sleep(@max_time - diff * @min_time)
    end

    new_state = check(%{state | block_id: current_block_id}, 1)

    {:reply, new_state.candidate, new_state}
  end

  @impl true
  def handle_cast({:complete, db_ref}, %{creator: creator_id} = state) do
    [_last_height, hash] = Block.last_created(creator_id, [-1, nil])

    {:noreply, %{state | candidate: nil, prev: hash}}
  end

  @impl true
  def handle_info(@message, state) do
    new_state = check(state, 0)
    {:noreply, new_state, {:continue, :next}}
  end

  defp check(
         %{candidate: nil, creator: creator_id, height: height, prev: prev} = state,
         priority
       ) do
    case BlockHandler.generate_files(creator_id, height, prev, priority) do
      nil ->
        state

      block ->
        new_height = height + 1
        :persistent_term.put(:height, new_height)

        %{state | candidate: block, height: new_height}
    end
  end

  defp check(state, _priority), do: state

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
  end
end
