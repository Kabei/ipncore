defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, BlockHandler, Round}
  require Block
  require Sqlite

  # @app Mix.Project.config()[:app]
  @module __MODULE__
  @pubsub :pubsub
  @time_to_wait 5_000
  @interval_check 2_500

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

    PubSub.subscribe(@pubsub, "block_timer")

    :timer.send_interval(@interval_check, :check)

    {:ok,
     %{
       block_id: block_id,
       from: nil,
       height: last_height + 1,
       prev: prev,
       tRef: nil,
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
  @spec get_next(block_id :: integer()) :: [map()] | []
  def get_next(block_id) do
    try do
      case GenServer.call(@module, {:get, block_id}, 7_000) do
        nil -> []
        candidate -> [candidate]
      end
    catch
      :exit, _ ->
        []
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
  def handle_call(
        :get,
        _from,
        %{candidate: candidate, vid: vid, height: height, prev: prev} = state
      ) do
    case candidate do
      nil ->
        case BlockHandler.generate_files(vid, height, prev) do
          nil ->
            {:reply, nil, state}

          block ->
            {:reply, block, %{state | candidate: block, height: height + 1, prev: block.hash}}
        end

      candidate ->
        {:reply, candidate, state}
    end
  end

  def handle_call({:get, _current_block_id}, _from, %{candidate: candidate} = state) do
    {:reply, candidate, state}
  end

  def handle_call(
        {:get_next, current_block_id},
        from,
        %{candidate: nil, tRef: tRef} = state
      ) do
    :timer.cancel(tRef)
    {:ok, tRef} = :timer.send_after(@time_to_wait, :finished)

    {:noreply, %{state | block_id: current_block_id, from: from, tRef: tRef}}
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

  @impl true
  def handle_cast(:check, %{height: height, prev: prev, vid: vid, tRef: tRef, from: from} = state) do
    if from != nil and tRef != nil do
      case BlockHandler.generate_files(vid, height, prev) do
        nil ->
          {:noreply, state}

        block ->
          :timer.cancel(tRef)
          GenServer.reply(from, [block])

          {:noreply,
           %{state | candidate: block, height: height + 1, prev: block.hash, tRef: nil, from: nil}}
      end
    else
      {:noreply, state}
    end
  end

  def handle_cast(:block, %{tRef: tRef, from: from} = state) do
    if from != nil and tRef != nil do
      :timer.cancel(tRef)
      GenServer.reply(from, [])

      {:noreply, %{state | tRef: nil, from: nil}}
    else
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(:finished, %{candidate: candidate, from: from} = state) do
    result =
      case candidate do
        nil -> []
        candidate -> [candidate]
      end

    GenServer.reply(from, result)
    {:noreply, %{state | tRef: nil}}
  end

  def handle_info(:check, %{candidate: candidate, vid: vid, height: height, prev: prev} = state) do
    case candidate do
      nil ->
        case BlockHandler.generate_files(vid, height, prev) do
          nil ->
            {:noreply, state}

          block ->
            {:noreply, %{state | candidate: block, height: height + 1, prev: block.hash}}
        end

      candidate ->
        {:noreply, state}
    end
  end

  # defp do_check(
  #        %{candidate: nil, vid: vid, height: height, prev: prev} = state,
  #        sleep \\ 0
  #      ) do
  #   case BlockHandler.generate_files(vid, height, prev) do
  #     nil ->
  #       if sleep > 0 do
  #         IO.inspect("Wait to #{sleep} ms")
  #         :timer.sleep(sleep)
  #       end

  #       state

  #     block ->
  #       new_height = height + 1

  #       %{state | candidate: block, height: new_height, prev: block.hash}
  #   end
  # end

  @impl true
  def terminate(_reason, _state) do
    :ok
  end
end
