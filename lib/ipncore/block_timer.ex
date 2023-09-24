defmodule BlockTimer do
  use GenServer, restart: :transient
  require SqliteStore

  @module __MODULE__
  @timeout Application.compile_env(:ipncore, :block_interval)
  @message :mine

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
  def init(%{conn: conn, stmts: stmts, creator: creator_id}) do
    [last_height, prev] =
      SqliteStore.fetch(conn, stmts, "last_block_created", [creator_id], [-1, nil])

    {:ok, %{creator: creator_id, height: last_height + 1, prev: prev, candidate: nil, tRef: nil},
     {:continue, :next}}
  end

  @impl true
  def handle_continue(:next, %{candidate: candidate} = state) do
    case candidate do
      nil ->
        {:ok, tRef} = :timer.send_after(@timeout, @message)

        {:noreply, %{state | tRef: tRef}, :hibernate}

      _ ->
        {:noreply, state, :hibernate}
    end
  end

  @spec get_block :: map() | nil
  def get_block do
    GenServer.call(@module, :get, :infinity)
  end

  @spec get_blocks :: [map()] | []
  def get_blocks do
    case GenServer.call(@module, :get, :infinity) do
      nil -> []
      candidate -> [candidate]
    end
  end

  @spec complete(conn :: reference(), stmts :: map()) :: :ok
  def complete(conn, stmts) do
    GenServer.cast(@module, {:complete, conn, stmts})
  end

  @spec stop :: :ok
  def stop do
    GenServer.stop(@module, :normal, :infinity)
  end

  @impl true
  def handle_call(:get, _from, %{tRef: tRef} = state) do
    :timer.cancel(tRef)
    new_state = check(state)
    {:reply, new_state.candidate, new_state, {:continue, :next}}
  end

  @impl true
  def handle_cast(
        {:complete, conn, stmts},
        %{creator: creator_id, height: height, tRef: tRef} = state
      ) do
    [last_height, hash] =
      SqliteStore.fetch(conn, stmts, "last_block_created", [creator_id], [-1, nil])

    if last_height >= height do
      :timer.cancel(tRef)

      {:noreply, %{state | candidate: nil, height: last_height + 1, prev: hash},
       {:continue, :next}}
    else
      {:noreply, %{state | height: last_height + 1, prev: hash}, {:continue, :next}}
    end
  end

  @impl true
  def handle_info(@message, state) do
    new_state = check(state)
    {:noreply, new_state, {:continue, :next}}
  end

  defp check(%{candidate: candidate, creator: creator_id, height: height, prev: prev} = state) do
    candidate =
      case candidate do
        nil ->
          case BlockHandler.generate_files(creator_id, height, prev) do
            nil ->
              nil

            block ->
              block
          end

        x ->
          x
      end

    %{state | candidate: candidate}
  end

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
  end
end
