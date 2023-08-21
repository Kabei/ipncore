defmodule Store.Sqlite2 do
  alias Exqlite.Sqlite3NIF

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @base opts[:base]
      @name opts[:base] |> to_string()
      @create opts[:create]
      @alter opts[:alter] || []
      @attach opts[:attach] || []
      @version opts[:version] || 0
      @stmts opts[:stmt]
      @foreign_key opts[:foreign] || ~c"OFF"

      require Logger
      alias Exqlite.Sqlite3NIF
      alias Exqlite.Sqlite3
      alias Ippan.Utils
      use GenServer, restart: :transient

      def child_spec(opts) do
        state = start(opts)

        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [state]},
          restart: :transient
        }
      end

      def start_link(_, path) when is_binary(path) do
        state = start(path)

        GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: @base)
      end

      def start_link(path) when is_binary(path) do
        state = start(path)
        GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: @base)
      end

      def start_link(state) do
        GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: @base)
      end

      defmacrop call(request, timeout \\ :infinity) do
        quote do
          :gen_server.call(@base, unquote(request), unquote(timeout))
        end
      end

      defmacrop cast(request) do
        quote do
          :gen_server.cast(@base, unquote(request))
        end
      end

      def name, do: @name

      def statments, do: @stmts

      def start(path) when is_binary(path) do
        {:ok, conn} = open(path)

        start(conn)
      end

      def start(conn) when is_reference(conn) do
        :ok = create(conn)

        attach(conn)

        statements =
          for {name, sql} <- @stmts, into: %{} do
            {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
            {name, statement}
          end

        check_version(conn)

        %{conn: conn, stmt: statements}
      end

      def start(%{conn: conn, stmt: statements} = state), do: state

      defp attach(conn) do
        for {key, path} <- @attach do
          Sqlite3NIF.execute(conn, ~c"ATTACH DATABASE '#{path}' AS '#{key}'")
        end
      end

      @impl true
      def init(state) do
        begin(state.conn)
        {:ok, state}
      end

      @spec open(binary()) :: {:ok, reference()}
      def open(path) do
        path
        |> Path.dirname()
        |> File.mkdir_p()

        # flags = [:sqlite_open_sharedcache]
        {:ok, conn} = Sqlite3.open(path, [])
        Sqlite3NIF.execute(conn, ~c"PRAGMA foreign_keys = #{@foreign_key}")
        Sqlite3NIF.execute(conn, ~c"PRAGMA journal_mode = WAL")
        Sqlite3NIF.execute(conn, ~c"PRAGMA synchronous = 1")
        Sqlite3NIF.execute(conn, ~c"PRAGMA cache_size = -100000000")
        Sqlite3NIF.execute(conn, ~c"PRAGMA temp_store = memory")
        Sqlite3NIF.execute(conn, ~c"PRAGMA mmap_size = 30000000000")
        Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
        {:ok, conn}
      end

      # @spec open_readonly(binary()) :: {:ok, term()}
      # def open_readonly(path) do
      #   flags = [:sqlite_open_readonly, :sqlite_open_uri]

      #   {:ok, conn} = Sqlite3.open(path, flags)
      #   Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
      #   {:ok, conn}
      # end

      # def open_memory do
      #   {:ok, conn} = Sqlite3.open(":memory:")
      #   Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
      #   {:ok, conn}
      # end

      def begin do
        call({:execute, ~c"BEGIN"})
      end

      def commit do
        call({:execute, ~c"COMMIT"})
      end

      def begin(conn) do
        Sqlite3NIF.execute(conn, ~c"BEGIN")
      end

      def commit(conn) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")
      end

      def alter(conn, _version) do
        :ok
      end

      defp check_version(conn) do
        {:ok, stmt} = Sqlite3NIF.prepare(conn, ~c"PRAGMA USER_VERSION")
        {:row, [v]} = Sqlite3NIF.step(conn, stmt)
        Sqlite3NIF.release(conn, stmt)

        cond do
          v > @version ->
            raise "Bad database API version"

          v < @version ->
            :ok = alter(conn, @version)
            Sqlite3NIF.execute(conn, ~c"PRAGMA USER_VERSION #{@version}")
            :ok

          true ->
            :ok
        end
      end

      def create(conn) do
        for sql <- List.wrap(@create) do
          Sqlite3NIF.execute(conn, sql)
        end

        :ok
      end

      def insert(params) do
        cast({:step, "insert", params})
      end

      @spec insert_sync(list()) :: integer() | :busy | {:error, term()}
      def insert_sync(params) do
        call({:step, "insert", params})
      end

      def one(key) when is_list(key) do
        case call({:step, "lookup", key}) do
          {:row, []} -> nil
          {:row, data} -> data
          _ -> nil
        end
      end

      def one(key) do
        case call({:step, "lookup", [key]}) do
          {:row, []} -> nil
          {:row, data} -> data
          _ -> nil
        end
      end

      def exists?(key) do
        {:row, [1]} == call({:step, "exists", [key]})
      end

      def owner?(key, owner) do
        {:row, [1]} == call({:step, "owner", [key, owner]})
      end

      def delete(params) do
        call({:step, "delete", params})
      end

      @spec update(map(), Keyword.t()) ::
              non_neg_integer() | {:error, term()}
      def update(map_set_fields, map_where) do
        {fields, values} = Utils.rows_to_columns(map_set_fields)
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

        call({:update, @name, set_fields, where, values ++ w_values})
      end

      def step(name, params) do
        call({:step, name, params})
      end

      def fetch(name, params) do
        call({:fetch, name, params})
      end

      def all(limit \\ 100, offset \\ 0) do
        call({:execute, :fetch, ~c"SELECT * FROM #{@name} LIMIT ? OFFSET ?", [limit, offset]})
      end

      def step_change(name, params) do
        call({:change, name, params})
      end

      def changes(conn) do
        Sqlite3NIF.changes(conn)
      end

      def run(fun) do
        call({:run, fun})
      end

      def savepoint(id) do
        call({:execute, ~c"SAVEPOINT #{id}"})
      end

      def sv_release(id) do
        call({:execute, ~c"RELEASE #{id}"})
      end

      def sv_rollback(id) do
        call({:execute, ~c"ROLLBACK TO #{id}"})
      end

      def sync do
        call(:sync)
      end

      def checkpoint do
        call(
          {:run,
           fn %{conn: conn, stmt: stmts} = state ->
             Sqlite3NIF.execute(conn, ~c"COMMIT")

             Enum.each(stmts, fn {_, stmt} ->
               Sqlite3NIF.release(conn, stmt)
             end)

             Sqlite3NIF.execute(conn, ~c"PRAGMA wal_checkpoint(TRUNCATE)")
             Sqlite3NIF.execute(conn, ~c"VACUUM")

             statements =
               for {name, sql} <- @stmts, into: %{} do
                 {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
                 {name, statement}
               end

             Sqlite3NIF.execute(conn, ~c"BEGIN")
             {:reply, :ok, Map.put(state, :stmt, statements)}
           end}
        )
      end

      def get_state do
        call(:state)
      end

      @impl true
      def handle_cast(
            {:step, stmt_name, params},
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        Sqlite3NIF.bind_step(conn, statement, params)
        {:noreply, state}
      end

      def handle_cast({:execute, sql, params}, %{conn: conn} = state) do
        {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
        Sqlite3NIF.bind_step(conn, statement, params)
        Sqlite3NIF.release(conn, statement)
        {:noreply, state}
      end

      def handle_cast({:execute, sql}, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, sql)
        {:noreply, state}
      end

      @impl true
      def handle_call(
            {:step, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        {:reply, Sqlite3NIF.bind_step(conn, statement, params), state}
      end

      def handle_call(
            {:fetch, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        Sqlite3NIF.bind(conn, statement, params)
        result = Sqlite3.fetch_all(conn, statement)
        {:reply, result, state}
      end

      def handle_call(
            {:change, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        {:reply, Sqlite3NIF.bind_step_changes(conn, statement, params), state}
      end

      def handle_call({:execute, sql}, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, sql)
        {:reply, :ok, state}
      end

      def handle_call({:execute, command, query, args}, _from, %{conn: conn, stmt: stmt} = state) do
        {:ok, statement} = Sqlite3NIF.prepare(conn, query)

        result =
          case command do
            :step ->
              Sqlite3NIF.bind_step(conn, statement, args)

            :fetch ->
              Sqlite3NIF.bind(conn, statement, args)
              Sqlite3.fetch_all(conn, statement)

            :change ->
              Sqlite3NIF.bind_step(conn, statement, args)
              Sqlite3NIF.changes(conn)
          end

        Sqlite3NIF.release(conn, statement)

        {:reply, result, state}
      end

      def handle_call({:run, fun}, _from, state) do
        try do
          fun.(state)
        rescue
          _ex ->
            {:reply, :error, state}
        end
      end

      def handle_call(
            {:update, table, set_fields, where, values_list},
            _from,
            %{conn: conn} = state
          ) do
        {:ok, statement} =
          Sqlite3NIF.prepare(conn, ~c"UPDATE #{table} SET #{set_fields} WHERE #{where}")

        n = Sqlite3NIF.bind_step_changes(conn, statement, values_list)
        Sqlite3NIF.release(conn, statement)

        {:reply, n, state}
      end

      def handle_call(:sync, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")
        Sqlite3NIF.execute(conn, ~c"BEGIN")
        {:reply, :ok, state}
      end

      def handle_call(:state, _from, %{conn: conn, stmt: stmts} = state) do
        {:reply, {conn, stmts}, state}
      end

      @impl true
      def terminate(_reason, %{conn: conn, stmt: stmts} = state) do
        stop(conn, stmts)
      end

      def stop(conn, stmts) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")

        Enum.each(stmts, fn {_, stmt} ->
          Sqlite3NIF.release(conn, stmt)
        end)

        Sqlite3NIF.execute(conn, ~c"PRAGMA wal_checkpoint(TRUNCATE)")
        Sqlite3NIF.execute(conn, ~c"VACUUM;")
        Sqlite3NIF.execute(conn, ~c"PRAGMA analysis_limit=400;")
        Sqlite3NIF.execute(conn, ~c"PRAGMA optimize;")

        Sqlite3NIF.close(conn)
      end

      defoverridable init: 1,
                     stop: 2,
                     one: 1,
                     all: 2,
                     insert: 1,
                     insert_sync: 1,
                     update: 2,
                     delete: 1,
                     exists?: 1,
                     owner?: 2,
                     alter: 2,
                     terminate: 2
    end
  end
end
