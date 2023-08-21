defmodule SqliteStore do
  alias Exqlite.{Sqlite3, Sqlite3NIF}

  defmacro step(conn, stmt, params) do
    quote do
      Sqlite3NIF.bind_step(unquote(conn), unquote(stmt), unquote(params))
    end
  end

  defmacro step_change(conn, stmt, params) do
    quote do
      Sqlite3NIF.bind_step_changes(unquote(conn), unquote(stmt), unquote(params))
    end
  end

  defmacro execute(conn, sql) do
    quote do
      Sqlite3NIF.execute(unquote(conn), unquote(sql))
    end
  end

  defmacro exists?(conn, stmt, id) do
    quote do
      {:row, [1]} == Sqlite3NIF.step(unquote(conn), unquote(stmt), unquote([id]))
    end
  end

  defmacro get(conn, stmt, id) do
    quote do
      case Sqlite3NIF.step(unquote(conn), unquote(stmt), unquote([id])) do
        {:row, []} -> nil
        {:row, data} -> data
        _ -> nil
      end
    end
  end

  defmacro fetch_all(conn, stmt, limit \\ 100, offset \\ 0) do
    quote do
      Sqlite3.fetch_all(unquote(conn), unquote(stmt), [unquote(limit), unquote(offset)])
    end
  end

  defmacro savepoint(conn, id) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"SAVEPOINT #{unquote(id)}")
    end
  end

  defmacro release(conn, id) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"RELEASE #{unquote(id)}")
    end
  end

  defmacro rollback_to(conn, id) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"ROLLBACK TO #{unquote(id)}")
    end
  end

  defmacro rollback_to(conn) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"ROLLBACK")
    end
  end

  defmacro commit(conn) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"COMMIT")
    end
  end

  defmacro begin(conn) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"BEGIN")
    end
  end

  @spec check_version(String.t(), list(), list(), integer()) :: :ok | {:error, term()}
  def check_version(filename, creation_sql, alter_sql, new_version) do
    case File.exists?(filename) do
      true ->
        {:ok, conn} = Sqlite3.open(filename, [])
        {:ok, stmt} = Sqlite3NIF.prepare(conn, ~c"PRAGMA USER_VERSION")
        {:row, [old_version]} = Sqlite3NIF.step(conn, stmt)
        Sqlite3NIF.release(conn, stmt)

        cond do
          old_version == new_version ->
            Sqlite3NIF.close(conn)

          new_version == old_version + 1 ->
            for sql <- alter_sql do
              Sqlite3NIF.execute(conn, sql)
            end

            Sqlite3NIF.execute(conn, ~c"PRAGMA USER_VERSION #{new_version}")
            Sqlite3NIF.close(conn)

          true ->
            Sqlite3NIF.close(conn)
            raise "Bad version #{filename} v#{old_version} -> #{new_version}"
        end

      _false ->
        {:ok, conn} = Sqlite3.open(filename, [])

        for sql <- creation_sql do
          Sqlite3NIF.execute(conn, sql)
        end

        Sqlite3NIF.close(conn)
    end
  end

  def open(filename, attaches \\ []) do
    {:ok, conn} = Sqlite3.open(filename, [])
    attach(conn, attaches)
    setup(conn)
    begin(conn)

    {:ok, conn}
  end

  def prepare_statements(conn, statements) do
    for {name, sql} <- statements, into: %{} do
      {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
      {name, statement}
    end
  end

  def release_statements(conn, statements) do
    Enum.each(statements, fn stmt ->
      Sqlite3NIF.release(conn, stmt)
    end)
  end

  def attach(conn, map) do
    for {name, filename} <- map do
      Sqlite3NIF.execute(conn, ~c"ATTACH DATABASE '#{filename}' AS '#{name}'")
    end
  end

  defp setup(conn) do
    Sqlite3NIF.execute(conn, ~c"PRAGMA foreign_keys = OFF")
    Sqlite3NIF.execute(conn, ~c"PRAGMA journal_mode = WAL")
    Sqlite3NIF.execute(conn, ~c"PRAGMA synchronous = 1")
    Sqlite3NIF.execute(conn, ~c"PRAGMA cache_size = -100000000")
    Sqlite3NIF.execute(conn, ~c"PRAGMA temp_store = memory")
    Sqlite3NIF.execute(conn, ~c"PRAGMA mmap_size = 30000000000")
    Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
  end
end
