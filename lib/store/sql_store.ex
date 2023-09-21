defmodule SqliteStore do
  alias Exqlite.{Sqlite3, Sqlite3NIF}

  defmacro one(conn, stmts, name, args \\ [], default \\ nil) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, args: args, default: default],
          location: :keep do
      case Sqlite3NIF.bind_step(conn, Map.get(stmts, name), args) do
        {:row, [n]} ->
          n

        :done ->
          default
      end
    end
  end

  defmacro update(conn, table_name, map_fields, map_where) do
    quote bind_quoted: [
            conn: conn,
            map_fields: map_fields,
            map_where: map_where,
            table_name: table_name
          ],
          location: :keep do
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

  defmacro step(conn, stmts, name, args \\ []) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, args: args], location: :keep do
      Sqlite3NIF.bind_step(conn, Map.get(stmts, name), args)
    end
  end

  defmacro step_change(conn, stmts, name, args) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, args: args], location: :keep do
      Sqlite3NIF.bind_step_changes(conn, Map.get(stmts, name), args)
    end
  end

  defmacro execute(conn, sql) do
    quote do
      Sqlite3NIF.execute(unquote(conn), unquote(sql))
    end
  end

  defmacro exists?(table, conn, stmts, name, id) do
    quote bind_quoted: [table: table, conn: conn, stmts: stmts, name: name, id: id],
          location: :keep do
      case :ets.member(table, id) do
        true ->
          true

        false ->
          SqliteStore.exists?(conn, stmts, name, [id])
      end
    end
  end

  defmacro exists?(conn, stmts, name, args) when is_list(args) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, args: args], location: :keep do
      {:row, [1]} == Sqlite3NIF.bind_step(conn, Map.get(stmts, name), args)
    end
  end

  defmacro exists?(conn, stmts, name, id) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, id: id], location: :keep do
      {:row, [1]} == Sqlite3NIF.bind_step(conn, Map.get(stmts, name), [id])
    end
  end

  defmacro fetch(conn, stmts, name, args, default \\ nil) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, args: args, default: default],
          location: :keep do
      case Sqlite3NIF.bind_step(conn, Map.get(stmts, name), args) do
        {:row, []} -> default
        {:row, data} -> data
        _ -> default
      end
    end
  end

  defmacro lookup_map(table, conn, stmts, name, id, mod_format) do
    quote bind_quoted: [
            table: table,
            conn: conn,
            stmts: stmts,
            name: name,
            id: id,
            mod_format: mod_format
          ],
          location: :keep do
      case :ets.lookup(table, id) do
        [{_, map}] ->
          map

        [] ->
          args = if(is_tuple(id), do: Tuple.to_list(id), else: [id])

          case Sqlite3NIF.bind_step(conn, Map.get(stmts, name), args) do
            {:row, []} ->
              nil

            {:row, data} ->
              {_, map} = result = mod_format.list_to_tuple(data)
              :ets.insert(table, result)

              if :ets.info(table, :size) > 10_000_000 do
                :ets.delete(table, :ets.first(table))
              end

              map

            _ ->
              nil
          end
      end
    end
  end

  defmacro fetch_all(conn, stmts, name, limit \\ 100, offset \\ 0) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name, limit: limit, offset: offset],
          location: :keep do
      stmt = Map.get(stmts, name)
      Sqlite3NIF.bind(conn, stmt, [limit, offset])

      case Sqlite3.fetch_all(conn, stmt, 100) do
        {:ok, data} -> data
        _ -> []
      end
    end
  end

  defmacro all(conn, stmts, name) do
    quote bind_quoted: [conn: conn, stmts: stmts, name: name], location: :keep do
      case Sqlite3.fetch_all(conn, Map.get(stmts, name), 100) do
        {:ok, data} -> data
        _ -> []
      end
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
      if not String.contains?(filename, "?") do
        path = Path.join(base, filename)
        # IO.inspect(path)
        creation = Map.get(creations, name, [])
        {:ok, conn} = Sqlite3.open(path, [])

        for sql <- creation do
          :ok = Sqlite3NIF.execute(conn, sql)
        end

        Sqlite3NIF.close(conn)
      end
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
