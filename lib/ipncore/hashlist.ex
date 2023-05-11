# defmodule RequestStore do
#   @table "request"
#   use Store.Sqlite,
#     base: :rp,
#     table: @table,
#     create: """
#     CREATE TABLE IF NOT EXISTS #{@table}(
#       hash BLOB PRIMARY KEY,
#       timestamp UNSIGNED BIGINT NOT NULL,
#       type INTEGER,
#       id BLOB,
#       message BLOB,
#       size UNSIGNED BIGINT
#     ) WITHOUT ROWID;
#     """,
#     stmt: %{
#       insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5)",
#       lookup: "SELECT * FROM #{@table} WHERE hash = ?",
#       lookup_hash: "SELECT * FROM #{@table} WHERE hash = ? LIMIT 1",
#       exists: "SELECT 1 FROM #{@table} WHERE hash = ?",
#       delete: "DELETE FROM #{@table} WHERE hash = ?"
#     }

# end

defmodule HashList do
  # use GenServer
# require Logger
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

  # require Logger
  def lookup!(ets, key, hash, timestamp) do
    # r = :ets.lookup(ets, key)
    #Logger.debug(inspect(r))
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
        :ok
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
