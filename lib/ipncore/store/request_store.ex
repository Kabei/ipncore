defmodule RequestStore do
  alias Ippan.Block
  use GenServer

  @module :requests
  @otp_app :ipncore

  def start_link(opts) do
    GenServer.start_link(__MODULE__, nil, [name: @module, hibernate_after: 5_000] ++ opts)
  end

  @impl true
  def init(_init_arg) do
    ets = :ets.new(@module, [:duplicate_bag, :public, :named_table])
    {:ok, %{ets: ets, fallback: %{}, sync: false, pid: self()}}
  end

  defmacrop call(server, request, timeout \\ :infinity) do
    quote do
      GenServer.call(unquote(server), unquote(request), unquote(timeout))
    end
  end

  def insert(hash, request) do
    GenServer.cast(@module, {:insert, hash, request})
    # :ets.insert(@module, {hash, request})
  end

  @doc """
  Create a new block
  """
  def save(%Block{} = block) do
    call(@module, {:sync, block})
  end

  @impl true
  def handle_cast({:insert, hash, request}, %{fallback: fallback, sync: true} = state) do
    {:noreply, %{state | fallback: Map.put(fallback, hash, request)}}
  end

  def handle_cast({:insert, hash, request}, %{ets: ets} = state) do
    :ets.insert(ets, {hash, request})
    {:noreply, state}
  end

  @impl true
  def handle_info(:sync_complete, %{ets: ets, fallback: fallback} = state) do
    :ets.insert(ets, Map.to_list(fallback))
    {:noreply, %{state | sync: false, fallback: %{}}}
  end

  @impl true
  def handle_call({:sync, block}, _from, %{ets: ets, pid: pid} = state) do
    spawn_link(fn ->
      Process.flag(:priority, :low)

      data_dir = Application.get_env(@otp_app, :data_dir, "data")
      path = Path.join(data_dir, "blocks/#{block.id}.block")

      dataset =
        :ets.tab2list(ets)
        |> :erlang.term_to_binary(compressed: 9)

      {:ok, file} = :file.open(to_charlist(path), [:write, :raw])
      :file.write(file, dataset)
      :file.close(file)

      :ets.delete_all_objects(ets)
      send(pid, :sync_complete)
    end)

    {:reply, :ok, %{state | sync: true}}
  end
end
