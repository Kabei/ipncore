defmodule SqliteStore do
  alias Exqlite.{Sqlite3, Sqlite3NIF}

  defmacro total(conn, stmts, name, params \\ []) do
    quote do
      {:row, [n]} =
        Sqlite3NIF.bind_step(
          unquote(conn),
          Map.get(unquote(stmts), unquote(name)),
          unquote(params)
        )

      n
    end
  end

  defmacro update(conn, table_name, map_fields, map_where) do
    quote bind_quoted: [
            conn: conn,
            map_fields: map_fields,
            map_where: map_where,
            table_name: table_name
          ] do
      {fields, values} = Ippan.Utils.rows_to_columns(map_fields)
      {w_fields, w_values} = Ippan.Utils.rows_to_columns(map_where)

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
        Sqlite3NIF.prepare(conn, ~c"UPDATE #{table_name} SET #{set_fields} WHERE #{where}")

      n = Sqlite3NIF.bind_step(conn, statement, values ++ w_values)
      Sqlite3NIF.release(conn, statement)
    end
  end

  defmacro step(conn, stmts, name, params) do
    quote do
      Sqlite3NIF.bind_step(unquote(conn), Map.get(unquote(stmts), unquote(name)), unquote(params))
    end
  end

  defmacro step_change(conn, stmts, name, params) do
    quote do
      Sqlite3NIF.bind_step_changes(
        unquote(conn),
        Map.get(unquote(stmts), unquote(name)),
        unquote(params)
      )
    end
  end

  defmacro execute(conn, sql) do
    quote do
      Sqlite3NIF.execute(unquote(conn), unquote(sql))
    end
  end

  defmacro exists?(conn, stmts, stmt_name, args) when is_list(args) do
    quote do
      {:row, [1]} ==
        Sqlite3NIF.bind_step(
          unquote(conn),
          Map.get(unquote(stmts), unquote(stmt_name)),
          unquote(args)
        )
    end
  end

  defmacro exists?(conn, stmts, stmt_name, id) do
    quote do
      {:row, [1]} ==
        Sqlite3NIF.bind_step(
          unquote(conn),
          Map.get(unquote(stmts), unquote(stmt_name)),
          unquote([id])
        )
    end
  end

  defmacro get(conn, stmt, id) do
    quote do
      case Sqlite3NIF.bind_step(unquote(conn), unquote(stmt), unquote([id])) do
        {:row, []} -> nil
        {:row, data} -> data
        _ -> nil
      end
    end
  end

  defmacro lookup(table, conn, stmts, stmt_name, id) do
    quote do
      case :ets.lookup(unquote(table), unquote(id)) do
        [x] ->
          x

        [] ->
          case Sqlite3NIF.bind_step(
                 unquote(conn),
                 Map.get(unquote(stmts), unquote(stmt_name)),
                 unquote([id])
               ) do
            {:row, []} ->
              nil

            {:row, data} ->
              :ets.insert(unquote(table), List.to_tuple(data))

              if :ets.info(unquote(table), :size) > 10_000_000 do
                :ets.delete(unquote(table), :ets.first(unquote(table)))
              end

              data

            _ ->
              nil
          end
      end
    end
  end

  defmacro lookup_map(table, conn, stmts, stmt_name, params, mod_format) do
    quote do
      case :ets.lookup(unquote(table), unquote(params)) do
        [x] ->
          x

        [] ->
          case Sqlite3NIF.bind_step(
                 unquote(conn),
                 Map.get(unquote(stmts), unquote(stmt_name)),
                 [unquote(params)]
               ) do
            {:row, []} ->
              nil

            {:row, data} ->
              :ets.insert(unquote(table), List.to_tuple(data))

              if :ets.info(unquote(table), :size) > 10_000_000 do
                :ets.delete(unquote(table), :ets.first(unquote(table)))
              end

              unquote(mod_format).to_map(data)

            _ ->
              nil
          end
      end
    end
  end

  defmacro fetch_all(conn, stmts, name, limit \\ 100, offset \\ 0) do
    quote do
      Sqlite3.fetch_all(unquote(conn), Map.get(unquote(stmts), unquote(name)), [
        unquote(limit),
        unquote(offset)
      ])
    end
  end

  defmacro all(conn, stmts, name) do
    quote do
      Sqlite3.fetch_all(unquote(conn), Map.get(unquote(stmts), unquote(name)), [])
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

  defmacro sync(conn) do
    quote do
      Sqlite3NIF.execute(unquote(conn), ~c"COMMIT")
      Sqlite3NIF.execute(unquote(conn), ~c"BEGIN")
    end
  end

  @spec check_version(term(), list(), integer()) :: :ok | {:stop, term(), term()}
  def check_version(conn, alter_sql, new_version) do
    {:ok, stmt} = Sqlite3NIF.prepare(conn, ~c"PRAGMA USER_VERSION")
    {:row, [old_version]} = Sqlite3NIF.step(conn, stmt)
    Sqlite3NIF.release(conn, stmt)

    cond do
      old_version == new_version ->
        :ok

      new_version == old_version + 1 ->
        for sql <- alter_sql do
          Sqlite3NIF.execute(conn, sql)
        end

        Sqlite3NIF.execute(conn, ~c"PRAGMA USER_VERSION #{new_version}")
        :ok

      true ->
        Sqlite3NIF.close(conn)
        {:stop, :normal, "Bad version v#{old_version}"}
    end
  end

  def open_setup(main_name, main_file, creations, attaches) do
    # create attach databases
    base = Path.dirname(main_file)

    for {name, filename} <- attaches do
      path = Path.join(base, filename)
      IO.inspect(path)
      creation = Map.get(creations, name, [])
      {:ok, conn} = Sqlite3.open(path, [])

      for sql <- creation do
        :ok = Sqlite3NIF.execute(conn, sql)
      end

      Sqlite3NIF.close(conn)
    end

    # create main database
    {:ok, conn} = Sqlite3.open(main_file, [])
    creation = Map.get(creations, main_name, [])

    for sql <- creation do
      :ok = Sqlite3NIF.execute(conn, sql)
    end

    # config main database
    setup(conn)
    # attach databases
    attach(conn, base, attaches)

    {:ok, conn}
  end

  def prepare_statements(conn, statements) do
    map =
      for {name, sql} <- statements, into: %{} do
        {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
        {name, statement}
      end

    {:ok, map}
  end

  def release_statements(conn, statements) do
    Enum.each(statements, fn stmt ->
      Sqlite3NIF.release(conn, stmt)
    end)
  end

  def attach(conn, dirname, map) do
    for {name, filename} <- map do
      Sqlite3NIF.execute(conn, ~c"ATTACH DATABASE '#{Path.join(dirname, filename)}' AS '#{name}'")
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
