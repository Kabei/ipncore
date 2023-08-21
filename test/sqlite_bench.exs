defmodule Bench do
  alias Exqlite.Sqlite3NIF
  alias Exqlite.Sqlite3

  @cpus System.schedulers_online()

  def insert_test(item_size \\ 50_000, cpus \\ @cpus) do
    {:ok, conn} = Sqlite3.open(":memory:", [])
    # {:ok, conn} = Sqlite3.open("wallet.test", [])
    setup(conn)
    Sqlite3NIF.execute(conn, ~c"CREATE TABLE IF NOT EXISTS wallet(
      id BLOB PRIMARY KEY NOT NULL,
      pubkey BLOB,
      validator BIGINT,
      created_at BIGINT
      ) WITHOUT ROWID;")

    time = :os.system_time(1000)
    Sqlite3NIF.execute(conn, ~c"BEGIN")

    data =
      for _ <- 0..(item_size - 1) do
        [:rand.bytes(20), :rand.bytes(32), :rand.uniform(100), time]
      end

    tasks =
      data
      |> Enum.chunk_every(div(item_size, cpus))
      |> Enum.map(fn x ->
        Task.async(fn ->
          {:ok, stmt} = Sqlite3NIF.prepare(conn, ~c"INSERT INTO wallet values(?1,?2,?3,?4)")

          for params <- x do
            Sqlite3NIF.bind_step(conn, stmt, params)
          end

          Sqlite3NIF.release(conn, stmt)
        end)
      end)

    tc(:run_tasks, [tasks])

    Sqlite3NIF.execute(conn, ~c"COMMIT")
    Sqlite3NIF.close(conn)
  end

  def insert_ets(item_size \\ 50_000, cpus \\ @cpus) do
    tid = :ets.new(:table, [:set, :public, read_concurrency: true, write_concurrency: true])

    time = :os.system_time(1000)

    data =
      for _ <- 0..item_size do
        {:rand.bytes(20), :rand.bytes(32), :rand.uniform(100), time}
      end

    tasks =
      data
      |> Enum.chunk_every(div(item_size, cpus))
      |> Enum.map(fn x ->
        Task.async(fn ->
          for object <- x do
            :ets.insert(tid, object)
          end
        end)
      end)

    tc(:run_tasks, [tasks])
    IO.inspect(:ets.info(tid))
    :ets.delete(tid)
  end

  def insert_dets(item_size \\ 50_000, cpus \\ @cpus) do
    {:ok, pid} = DetsPlus.open_file(:test_dets, file: "test.dets", auto_save: :infinity)
    time = :os.system_time(1000)

    data =
      for _ <- 0..(item_size - 1) do
        {:rand.bytes(20), :rand.bytes(32), :rand.uniform(100), time}
      end

    tasks =
      data
      |> Enum.chunk_every(div(item_size, cpus))
      |> Enum.map(fn x ->
        Task.async(fn ->
          for object <- x do
            DetsPlus.insert(pid, object)
          end
        end)
      end)

    tc(:run_tasks, [tasks])
    tc(:commit_dets, [pid])
    # DetsPlus.close(pid)
    GenServer.stop(pid)
  end

  def run_tasks(tasks) do
    Task.await_many(tasks, 10_000)

    :ok
  end

  def commit_dets(pid) do
    fun = DetsPlus.commit(pid)

    Task.async(fun)
    |> Task.await(:infinity)
  end

  defp setup(conn) do
    Sqlite3NIF.execute(conn, ~c"PRAGMA foreign_keys = OFF")
    Sqlite3NIF.execute(conn, ~c"PRAGMA journal_mode = WAL")
    Sqlite3NIF.execute(conn, ~c"PRAGMA synchronous = NORMAL")
    Sqlite3NIF.execute(conn, ~c"PRAGMA cache_size = -100000000")
    Sqlite3NIF.execute(conn, ~c"PRAGMA temp_store = memory")
    Sqlite3NIF.execute(conn, ~c"PRAGMA mmap_size = 30000000000")
    Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
  end

  defp tc(fun, args) do
    {time, :ok} = :timer.tc(__MODULE__, fun, args)

    IO.puts("#{div(time, 1000)}ms")
  end
end
