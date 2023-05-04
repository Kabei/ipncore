defmodule HashList do
  use GenServer

  @module __MODULE__

  def start_link(opts) do
    GenServer.start_link(@module, nil, Keyword.merge([name: @module], opts))
  end

  @impl true
  def init(_) do
    ets = :ets.new(:hashlist, [:set])
    {:ok, %{ets: ets}}
  end

  def insert(pid, object) do
    call(pid, {:insert, object})
  end

  def lookup(pid, key) do
    call(pid, {:lookup, key})
  end

  def delete_all_objects(pid) do
    GenServer.cast(pid, :delete_all_objects)
  end

  @impl true
  def handle_call({:insert, objects}, _from, %{ets: ets} = state) do
    :ets.insert(ets, objects)
    {:reply, :ok, state}
  end

  def handle_call({:lookup, key}, _from, %{ets: ets} = state) do
    case :ets.lookup(ets, key) do
      [{_key, value}] ->
        {:reply, value, state}

      _ ->
        {:reply, nil, state}
    end
  end

  @impl true
  def handle_cast(:delete_all_objects, %{ets: ets} = state) do
    :ets.delete_all_objects(ets)
    {:reply, state}
  end

  @impl true
  def terminate(_reason, %{ets: ets}) do
    :ets.delete(ets)
    :ok
  end

  defp call(pid, msg, timeout \\ :infinity) do
    GenServer.call(pid, msg, timeout)
  end
end
