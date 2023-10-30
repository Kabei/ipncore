defmodule Sqlite do
  alias Exqlite.{Sqlite3, Sqlite3NIF}

  defmacro one(name, args \\ [], default \\ nil) do
    quote bind_quoted: [name: name, args: args, default: default],
          location: :keep do
      stmt = :persistent_term.get({:stmt, name})

      case Sqlite3NIF.bind_step(var!(db_ref), stmt, args) do
        {:row, [n]} ->
          n

        :done ->
          default
      end
    end
  end

  defmacro update(table, map_fields, map_where) do
    quote bind_quoted: [
            map_fields: map_fields,
            map_where: map_where,
            table: table
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
        Sqlite3NIF.prepare(var!(db_ref), ~c"UPDATE #{table} SET #{set_fields} WHERE #{where}")

      n = Sqlite3NIF.bind_step(var!(db_ref), statement, values ++ w_values)
      Sqlite3NIF.release(var!(db_ref), statement)
      n
    end
  end

  defmacro step(name, args \\ []) do
    quote bind_quoted: [name: name, args: args], location: :keep do
      stmt = :persistent_term.get({:stmt, name})
      Sqlite3NIF.bind_step(var!(db_ref), stmt, args)
    end
  end

  defmacro step_change(name, args) do
    quote bind_quoted: [name: name, args: args], location: :keep do
      stmt = :persistent_term.get({:stmt, name})
      Sqlite3NIF.bind_step_changes(var!(db_ref), stmt, args)
    end
  end

  defmacro execute(db_ref, sql) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), unquote(sql))
    end
  end

  defmacro exists?(name, args) do
    quote bind_quoted: [name: name, args: args], location: :keep do
      stmt = :persistent_term.get({:stmt, name})
      {:row, [1]} == Sqlite3NIF.bind_step(var!(db_ref), stmt, args)
    end
  end

  defmacro fetch(name, args \\ [], default \\ nil) do
    quote bind_quoted: [name: name, args: args, default: default],
          location: :keep do
      stmt = :persistent_term.get({:stmt, name})

      case Sqlite3NIF.bind_step(var!(db_ref), stmt, args) do
        {:row, []} -> default
        {:row, data} -> data
        _ -> default
      end
    end
  end

  defmacro get(table, name, id, mod) do
    quote bind_quoted: [table: table, name: name, id: id, mod: mod],
          location: :keep do
      case :ets.lookup(table, id) do
        [{_, map}] ->
          map

        [] ->
          stmt = :persistent_term.get({:stmt, name})

          case Sqlite3NIF.bind_step(var!(db_ref), stmt, [id]) do
            {:row, []} ->
              nil

            {:row, data} ->
              {_, map} = result = mod.list_to_tuple(data)
              :ets.insert(table, result)

              map

            _ ->
              nil
          end
      end
    end
  end

  defmacro query(db, sql, args) do
    quote bind_quoted: [db: db, sql: sql, args: args] do
      {:ok, stmt} = Sqlite3NIF.prepare(db, to_charlist(sql))
      Sqlite3NIF.bind(db, stmt, args)
      res = Sqlite3.fetch_all(db, stmt)
      Sqlite3NIF.release(db, stmt)
      res
    end
  end

  defmacro fetch_all(name, args \\ []) do
    quote bind_quoted: [name: name, args: args],
          location: :keep do
      stmt = :persistent_term.get({:stmt, name})
      Sqlite3NIF.bind(var!(db_ref), stmt, args)

      case Sqlite3.fetch_all(var!(db_ref), stmt, 100) do
        {:ok, data} -> data
        _ -> []
      end
    end
  end

  defmacro all(name) do
    quote bind_quoted: [name: name], location: :keep do
      stmt = :persistent_term.get({:stmt, name})

      case Sqlite3.fetch_all(var!(db_ref), stmt, 100) do
        {:ok, data} -> data
        _ -> []
      end
    end
  end

  defmacro savepoint(db_ref, id) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"SAVEPOINT #{unquote(id)}")
    end
  end

  defmacro release(db_ref, id) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"RELEASE #{unquote(id)}")
    end
  end

  defmacro rollback_to(db_ref, id) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"ROLLBACK TO #{unquote(id)}")
    end
  end

  defmacro rollback(db_ref) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"ROLLBACK")
    end
  end

  defmacro commit(db_ref) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"COMMIT")
    end
  end

  defmacro begin(db_ref) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"BEGIN")
    end
  end

  defmacro sync(db_ref) do
    quote do
      Sqlite3NIF.execute(unquote(db_ref), ~c"COMMIT") |> IO.inspect()
      Sqlite3NIF.execute(unquote(db_ref), ~c"BEGIN") |> IO.inspect()
    end
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
    Enum.each(statements, fn {name, sql} ->
      {:ok, statement} = Sqlite3NIF.prepare(db_ref, sql)
      :persistent_term.put({prefix, name}, statement)
    end)
  end

  def release_statements(db_ref, statements, prefix) do
    Enum.each(statements, fn {name, _sql} ->
      key = {prefix, name}
      stmt = :persistent_term.get(key)
      Sqlite3NIF.release(db_ref, stmt)
      :persistent_term.erase(key)
    end)
  end

  def attach(db_ref, dirname, map) do
    for {name, filename} <- map do
      Sqlite3NIF.execute(
        db_ref,
        ~c"ATTACH DATABASE '#{Path.join(dirname, filename)}' AS '#{name}'"
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
end
