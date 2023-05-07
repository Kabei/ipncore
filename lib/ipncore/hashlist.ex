defmodule HashList do
  # use GenServer

  # @module __MODULE__

  def start(name) do
    # GenServer.start_link(@module, nil, Keyword.merge([name: @module, hibernate_after: 5_000], opts))
    :ets.new(name, [:duplicate_bag, :public, :named_table])
  end

  # @impl true
  # def init(_) do
  #   {:ok, %{ets: ets}}
  # end

  def insert(ets, object) do
    :ets.insert(ets, object)
  end

  def lookup!(ets, key, hash, timestamp) do
    case :ets.lookup(ets, key) do
      [{_key, {_, xhash}}] when hash == xhash ->
        raise IppanError, "Already exists"

      [{_key, {old_timestamp, old_hash, fallback}}] ->
        cond do
          old_timestamp < timestamp ->
            raise IppanError, "Already exists"

          old_hash < hash ->
            raise IppanError, "Already exists"

          true ->
            case fallback do
              {fun, args} ->
                apply(Ippan.Func.Fallback, fun, args)
                :ok

              _ ->
                :ok
            end
        end

      _ ->
        raise IppanError, "Already exists"
    end
  end

  def delete_all_objects(ets) do
    :ets.delete_all_objects(ets)
    # GenServer.cast(pid, :delete_all_objects)
  end

  # @impl true
  # def handle_call({:insert, objects}, _from, %{ets: ets} = state) do
  #   :ets.insert(ets, objects)
  #   {:reply, :ok, state}
  # end

  # def handle_call({:lookup, key}, _from, %{ets: ets} = state) do
  #   case :ets.lookup(ets, key) do
  #     [{_key, value}] ->
  #       {:reply, value, state}

  #     _ ->
  #       {:reply, nil, state}
  #   end
  # end

  # @impl true
  # def handle_cast(:delete_all_objects, %{ets: ets} = state) do
  #   :ets.delete_all_objects(ets)
  #   {:reply, state}
  # end

  # @impl true
  # def terminate(_reason, %{ets: ets}) do
  #   :ets.delete(ets)
  #   :ok
  # end

  # defp call(pid, msg, timeout \\ :infinity) do
  #   GenServer.call(pid, msg, timeout)
  # end
end
