defmodule Sqlite do
  alias Exqlite.{Sqlite3, Sqlite3NIF}
  alias Ippan.Utils

  def one({db_ref, thread}, name, args, default) do
    stmt = :persistent_term.get({:stmt, name, thread})

    case Sqlite3NIF.bind_step(db_ref, stmt, args) do
      {:row, [n]} ->
        n

      :done ->
        default
    end
  end

  def one(db_ref, name, args, default) do
    stmt = :persistent_term.get({:stmt, name, 0})

    case Sqlite3NIF.bind_step(db_ref, stmt, args) do
      {:row, [n]} ->
        n

      :done ->
        default
    end
  end

  def update(db_ref, table, map_fields, map_where) do
    {fields, values} = Utils.rows_to_columns(map_fields)
    {w_fields, w_values} = Utils.rows_to_columns(map_where)

    set_fields =
      for key <- fields do
        "#{key}=?"
      end
      |> Enum.join(", ")

    where =
      for key <- w_fields do
        "#{key}=?"
      end
      |> Enum.join(" AND ")

    {:ok, statement} =
      Sqlite3NIF.prepare(db_ref, ~c"UPDATE #{table} SET #{set_fields} WHERE #{where}")

    n = Sqlite3NIF.bind_step(db_ref, statement, values ++ w_values)
    Sqlite3NIF.release(db_ref, statement)
    n
  end

  def step({db_ref, thread}, name, args) do
    stmt = :persistent_term.get({:stmt, name, thread})
    Sqlite3NIF.bind_step(db_ref, stmt, args)
  end

  def step(db_ref, name, args) do
    stmt = :persistent_term.get({:stmt, name, 0})
    Sqlite3NIF.bind_step(db_ref, stmt, args)
  end

  def exists?({db_ref, thread}, name, args) do
    stmt = :persistent_term.get({:stmt, name, thread})
    {:row, [1]} == Sqlite3NIF.bind_step(db_ref, stmt, args)
  end

  def exists?(db_ref, name, args) do
    sql = :persistent_term.get({:sql, name})
    {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)
    res = {:row, [1]} == Sqlite3NIF.bind_step(db_ref, stmt, args)
    Sqlite3NIF.release(db_ref, stmt)
    res
  end

  def has?(db_ref, table, name, args) do
    if :ets.member(table, args) do
      true
    else
      sql = :persistent_term.get({:sql, name})
      {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)
      res = {:row, [1]} == Sqlite3NIF.bind_step(db_ref, stmt, args)
      Sqlite3NIF.release(db_ref, stmt)
      res
    end
  end

  def fetch(db_ref, name, args \\ [], default \\ nil) do
    sql = :persistent_term.get({:sql, name})
    {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)

    res =
      case Sqlite3NIF.bind_step(db_ref, stmt, args) do
        {:row, []} -> default
        {:row, data} -> data
        _ -> default
      end

    Sqlite3NIF.release(db_ref, stmt)
    res
  end

  def get(db_ref, table, name, id, mod) do
    case :ets.lookup(table, id) do
      [{_, map}] ->
        map

      [] ->
        sql = :persistent_term.get({:sql, name})
        {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)

        res =
          case Sqlite3NIF.bind_step(db_ref, stmt, [id]) do
            {:row, []} ->
              nil

            {:row, data} ->
              {_, map} = result = mod.list_to_tuple(data)
              :ets.insert(table, result)

              map

            _ ->
              nil
          end

        Sqlite3NIF.release(db_ref, stmt)

        res
    end
  end

  def query(db, sql, args) do
    {:ok, stmt} = Sqlite3NIF.prepare(db, to_charlist(sql))
    Sqlite3NIF.bind(db, stmt, args)
    res = Sqlite3.fetch_all(db, stmt)
    Sqlite3NIF.release(db, stmt)
    res
  end

  def fetch_all(db_ref, name, args \\ []) do
    sql = :persistent_term.get({:sql, name})
    {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)
    Sqlite3NIF.bind(db_ref, stmt, args)

    res =
      case Sqlite3.fetch_all(db_ref, stmt, 100) do
        {:ok, data} -> data
        _ -> []
      end

    Sqlite3NIF.release(db_ref, stmt)
    res
  end

  def all(db_ref, name) do
    sql = :persistent_term.get({:sql, name})
    {:ok, stmt} = Sqlite3NIF.prepare(db_ref, sql)

    res =
      case Sqlite3.fetch_all(db_ref, stmt, 100) do
        {:ok, data} -> data
        _ -> []
      end

    Sqlite3NIF.release(db_ref, stmt)
    res
  end

  def savepoint(db_ref, id) do
    Sqlite3NIF.execute(db_ref, ~c"SAVEPOINT #{id}")
  end

  def release(db_ref, id) do
    Sqlite3NIF.execute(db_ref, ~c"RELEASE #{id}")
  end

  def rollback_to(db_ref, id) do
    Sqlite3NIF.execute(db_ref, ~c"ROLLBACK TO #{id}")
  end

  def rollback(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"ROLLBACK")
  end

  def commit(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"COMMIT")
  end

  def begin(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"BEGIN")
  end

  def sync(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"COMMIT")
    Sqlite3NIF.execute(db_ref, ~c"BEGIN")
  end

  @spec check_version(term(), list(), integer()) :: :ok | {:stop, term(), term()}
  def check_version(db_ref, alter_sql, new_version) do
    {:ok, stmt} = Sqlite3NIF.prepare(db_ref, ~c"PRAGMA USER_VERSION")
    {:row, [old_version]} = Sqlite3NIF.step(db_ref, stmt)
    Sqlite3NIF.release(db_ref, stmt)

    cond do
      old_version == new_version ->
        :ok

      new_version == old_version + 1 ->
        for sql <- alter_sql do
          Sqlite3NIF.execute(db_ref, sql)
        end

        Sqlite3NIF.execute(db_ref, ~c"PRAGMA USER_VERSION #{new_version}")
        :ok

      true ->
        Sqlite3NIF.close(db_ref)
        {:stop, :normal, "Bad version v#{old_version}"}
    end
  end

  def open_setup(main_name, filename, creations, attaches) do
    # create attach databases
    base = Path.dirname(filename)

    for {name, filename} <- attaches do
      if not String.contains?(filename, "?") do
        path = Path.join(base, filename)
        # IO.inspect(path)
        creation = Map.get(creations, name, [])
        {:ok, db_ref} = Sqlite3.open(path, [])

        for sql <- creation do
          :ok = Sqlite3NIF.execute(db_ref, sql)
        end

        Sqlite3NIF.close(db_ref)
      end
    end

    # create main database
    {:ok, db_ref} = Sqlite3.open(filename, [])
    creation = Map.get(creations, main_name, [])

    for sql <- creation do
      :ok = Sqlite3NIF.execute(db_ref, sql)
    end

    # config main database
    setup(db_ref)
    # attach databases
    attach(db_ref, base, attaches)

    {:ok, db_ref}
  end

  def open_ro(filename, attaches) do
    base = Path.dirname(filename)
    {:ok, db_ref} = Sqlite3.open(filename, [])
    setup_ro(db_ref)
    attach(db_ref, base, attaches)
    {:ok, db_ref}
  end

  def prepare_statements(db_ref, statements, prefix) do
    cpus = System.schedulers_online() - 1

    Enum.each(statements, fn {name, sql} ->
      for n <- 0..cpus do
        {:ok, statement} = Sqlite3NIF.prepare(db_ref, sql)
        :persistent_term.put({prefix, name, n}, statement)
      end

      :persistent_term.put({:sql, name}, sql)
    end)
  end

  def release_statements(db_ref, statements, prefix) do
    cpus = System.schedulers_online() - 1

    Enum.each(statements, fn {name, _sql} ->
      for n <- 0..cpus do
        key = {prefix, name, n}
        stmt = :persistent_term.get(key)
        Sqlite3NIF.release(db_ref, stmt)
        :persistent_term.erase(key)
      end

      :persistent_term.erase({:sql, name})
    end)
  end

  def attach(db_ref, dirname, map) do
    for {name, filename} <- map do
      Sqlite3NIF.execute(
        db_ref,
        ~c"ATTACH DATABASE '#{:filename.join(dirname, filename)}' AS '#{name}'"
      )
    end
  end

  defp setup(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA foreign_keys = OFF")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA journal_mode = WAL")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA synchronous = 1")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA cache_size = -100000000")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA temp_store = memory")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA mmap_size = 30000000000")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA case_sensitive_like = ON")
  end

  defp setup_ro(db_ref) do
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA query_only = 1")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA foreign_keys = OFF")
    Sqlite3NIF.execute(db_ref, ~c"PRAGMA case_sensitive_like = ON")
  end

  defdelegate execute(db_ref, sql), to: Sqlite3NIF
end
