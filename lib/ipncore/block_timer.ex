defmodule BlockTimer do
  use GenServer
  alias Ippan.{Block, BlockHandler}
  require Block
  require Sqlite

  @module __MODULE__
  @time_to_wait 1_000
  @retry 5

  def start_link(args) do
    case System.get_env("test") do
      nil ->
        GenServer.start_link(@module, args, name: @module)

      _ ->
        :ignore
    end
  end

  @impl true
  def init(_) do
    vid = :persistent_term.get(:vid)
    db_ref = :persistent_term.get(:main_conn)

    block_id = Sqlite.one("last_block_id", [], -1) + 1

    %{hash: prev, height: last_height} =
      Block.last_created(vid)

    # :timer.send_interval(@interval_check, :auto_check)

    {:ok,
     %{
       block_id: block_id,
       candidate: [],
       from: nil,
       height: last_height + 1,
       prev: prev,
       tRef: nil,
       vid: vid
     }, :hibernate}
  end

  @doc """
  Get a candidate with a dynamic time to wait
  """
  # @spec get_block :: map() | nil
  @spec get_blocks :: [map()] | []
  def get_blocks do
    GenServer.call(@module, :get, :infinity)
  end

  # @doc """
  # Update block height, prev hash and candidate in state
  # """
  @spec complete(block_id :: integer(), blocks :: [map()]) :: :ok
  def complete(block_id, blocks) do
    GenServer.cast(@module, {:complete, block_id, blocks})
  end

  @spec stop :: :ok
  def stop do
    GenServer.stop(@module, :normal, :infinity)
  end

  @impl true
  def handle_call(:get, _from, state = %{candidate: []}) do
    do_check(state, @retry)
  end

  def handle_call(:get, _from, state = %{candidate: candidate}) do
    {:reply, candidate, state}
  end

  defp do_check(state = %{vid: vid, height: height, prev: prev}, retry) do
    case BlockHandler.generate_files(vid, height, prev) do
      nil ->
        if retry == 0 do
          {:reply, [], state}
        else
          :timer.sleep(@time_to_wait)
          do_check(state, retry - 1)
        end

      block ->
        {:reply, [block], %{state | candidate: [block], height: height + 1, prev: block.hash}}
    end
  end

  @impl true
  def handle_cast({:complete, last_block_id, blocks}, state = %{candidate: candidate}) do
    {:noreply,
     %{
       state
       | block_id: last_block_id,
         candidate:
           Map.filter(candidate, fn x ->
             not Enum.any?(blocks, fn y -> y.creator == x.creator and y.height == x.height end)
           end)
     }, :hibernate}
  end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
