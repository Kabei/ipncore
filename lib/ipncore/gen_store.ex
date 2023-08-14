defmodule GenStore do
  use GenServer
  alias Exqlite.Sqlite3NIF
  require Logger

  @module __MODULE__
  @table :table

  @ets_opts [
    :duplicate_bag,
    read_concurrency: true,
    write_concurrency: true
  ]

  @impl true
  def init(_) do
    tid = :ets.new(@table, @ets_opts)
    {:ok, %{tid: tid}}
  end

  def start_link(name) do
    GenServer.start_link(@module, nil, name: name, hibernate_after: 5_000)
  end

  def stop(pid) do
    GenServer.stop(pid)
  end

  def step(pid, module, stmt, params) do
    GenServer.cast(pid, {:step, module, stmt, params})
  end

  def raw(pid, module, stmt, params) do
    GenServer.cast(pid, {:raw, module, stmt, params})
  end

  def save(pid, filename) do
    GenServer.call(pid, {:save, filename})
  end

  def restore(filename) do
    dict = %{
      MessageStore.name() => MessageStore.get_state(),
      BlockStore.name() => BlockStore.get_state(),
      RoundStore.name() => RoundStore.get_state(),
      WalletStore.name() => WalletStore.get_state(),
      ValidatorStore.name() => ValidatorStore.get_state(),
      TokenStore.name() => TokenStore.get_state(),
      DomainStore.name() => DomainStore.get_state(),
      DnsStore.name() => DnsStore.get_state(),
      EnvStore.name() => EnvStore.get_state(),
      RefundStore.name() => RefundStore.get_state()
    }

    case :ets.file2tab(filename) do
      {:ok, data} ->
        data
        |> Stream.each(fn
          {"s", module, stmt_name, params} ->
            {conn, stmts} =
              Map.get(dict, module)

            statement = Map.get(stmts, stmt_name)
            Sqlite3NIF.bind_step(conn, statement, params)

          {"r", module, sql, params} ->
            {conn, _stmts} =
              Map.get(dict, module)

            {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
            Sqlite3NIF.bind_step(conn, statement, params)
            Sqlite3NIF.release(conn, statement)
        end)
        |> Stream.run()

        commit_all()

      _ ->
        :error
    end
  end

  def commit_all do
    Logger.debug("commit")
    MessageStore.sync()
    BlockStore.sync()
    RoundStore.sync()
    WalletStore.sync()
    BalanceStore.sync()
    ValidatorStore.sync()
    TokenStore.sync()
    DomainStore.sync()
    DnsStore.sync()
    EnvStore.sync()
    RefundStore.sync()
  end

  @impl true
  def handle_cast({:step, module, stmt, params}, %{tid: tid} = state) do
    :ets.insert(tid, {"s", module, stmt, params})
    {:noreply, state}
  end

  def handle_cast({:raw, sql, params}, %{tid: tid} = state) do
    :ets.insert(tid, {"r", sql, params})
    {:noreply, state}
  end

  @impl true
  def handle_call({:save, filename}, _from, %{tid: tid} = state) do
    :ets.tab2file(tid, filename)
    :ets.delete_all_objects(tid)
    {:reply, :ok, state}
  end

  @impl true
  def terminate(_reason, %{tid: tid}) do
    :ets.delete(tid)
    :ok
  end
end
