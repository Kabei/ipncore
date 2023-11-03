defmodule BlockTimer do
  use GenServer
  require Logger
  alias Ippan.{Block, BlockHandler}
  require Block
  require Sqlite

  # @app Mix.Project.config()[:app]
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
  def init(_) do
    vid = :persistent_term.get(:vid)
    db_ref = :persistent_term.get(:main_conn)

    block_id = Sqlite.one("last_block_id", [], -1) + 1

    [last_height, prev] =
      Block.last_created(vid, [-1, nil])

    load()

    {:ok,
     %{
       block_id: block_id,
       creator: vid,
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

    task =
      Task.async(fn -> check(%{state | block_id: current_block_id}, 1) end)

    cond do
      diff > 5 ->
        :timer.sleep(@min_time)

      diff <= 1 ->
        :timer.sleep(@max_time)

      true ->
        :timer.sleep(@max_time - diff * @min_time)
    end

    new_state = Task.await(task, :infinity)

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

  @filename "mem.data"
  # Create and load mempool table and counter
  defp load do
    dir = :persistent_term.get(:save_dir)
    filepath = Path.join(dir, "mem.data")
    # Create mempool counter
    cref = :counters.new(1, [])
    :persistent_term.put(:msg_counter, cref)

    if File.exists?(filepath) do
      {:ok, content} = File.read(filepath)

      case CBOR.Decoder.decode(content) do
        {%{"data" => data, "ix" => ix}, _rest} ->
          :ets.insert(:msg, data)
          :counters.put(cref, 1, ix)

        _other ->
          Logger.error("Error decode mem.data")
          {:error, :cbor_decoder_error}
      end

      File.rm(filepath)
    end
  end

  def save do
    size = :ets.info(:msg, :size)

    if size != 0 do
      dir = :persistent_term.get(:save_dir)
      filepath = Path.join(dir, @filename)
      data = :ets.tab2list(:msg)
      ix = :erlang.element(1, :lists.last(data))
      content = %{"msg" => data, "ix" => ix, "size" => size} |> CBOR.encode()
      File.write(filepath, content)
    end
  end
end
