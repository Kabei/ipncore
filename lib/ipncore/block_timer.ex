defmodule BlockTimer do
  use GenServer
  alias Ippan.{Block, BlockHandler, Round}
  require Block
  require Sqlite

  # @app Mix.Project.config()[:app]
  @module __MODULE__
  @min_time 750
  @max_time 2_000

  def start_link(args) do
    GenServer.start_link(@module, args, name: @module)
  end

  @impl true
  def init(_) do
    vid = :persistent_term.get(:vid)
    db_ref = :persistent_term.get(:main_conn)

    block_id = Sqlite.one("last_block_id", [], -1) + 1

    %{hash: prev, height: last_height} =
      Block.last_created(vid)

    {:ok,
     %{
       block_id: block_id,
       height: last_height + 1,
       prev: prev,
       vid: vid,
       candidate: nil
     }, :hibernate}
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
  @spec get_block(block_id :: integer()) :: [map()] | []
  def get_block(block_id) do
    case GenServer.call(@module, {:get, block_id}, :infinity) do
      nil -> []
      candidate -> [candidate]
    end
  end

  @doc """
  Update block height, prev hash and candidate in state
  """
  @spec complete(blocks :: list()) :: :ok
  def complete(blocks) do
    GenServer.cast(@module, {:complete, blocks})
  end

  @spec stop :: :ok
  def stop do
    GenServer.stop(@module, :normal, :infinity)
  end

  @impl true
  def handle_call(:get, _from, state) do
    {:reply, state.candidate, state}
  end

  def handle_call({:get, current_block_id}, _from, %{block_id: block_id, candidate: nil} = state) do
    diff = current_block_id - block_id

    sleep =
      cond do
        diff > 5 ->
          @min_time

        diff == 0 ->
          @max_time

        true ->
          @max_time - diff * @min_time
      end

    try do
      task =
        Task.async(fn -> do_check(%{state | block_id: current_block_id}, sleep) end)

      new_state = Task.await(task, 10_000)
      {:reply, new_state.candidate, new_state}
    catch
      :exit, _ ->
        {:reply, nil, state}
    end
  end

  def handle_call({:get, _current_block_id}, _from, %{candidate: candidate} = state) do
    {:reply, candidate, state}
  end

  @impl true
  def handle_continue(:check, state = %{candidate: nil}) do
    {:noreply, do_check(state), :hibernate}
  end

  def handle_continue(:check, state) do
    {:noreply, state, :hibernate}
  end

  @impl true
  def handle_cast({:complete, blocks}, %{vid: vid} = state) do
    if Round.is_some_block_mine?(blocks, vid) do
      # {:noreply, %{state | candidate: nil, prev: hash}, {:continue, :check}}
      {:noreply, %{state | candidate: nil}, :hibernate}
    else
      {:noreply, state, :hibernate}
      # {:noreply, %{state | prev: hash}, {:continue, :check}}
    end
  end

  defp do_check(
         %{candidate: nil, vid: vid, height: height, prev: prev} = state,
         sleep \\ 0
       ) do
    case BlockHandler.generate_files(vid, height, prev) do
      nil ->
        if sleep > 0 do
          IO.inspect("Wait to #{sleep} ms")
          :timer.sleep(sleep)
        end

        state

      block ->
        new_height = height + 1

        %{state | candidate: block, height: new_height, prev: block.hash}
    end
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
