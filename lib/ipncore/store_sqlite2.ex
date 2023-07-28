defmodule Store.Sqlite2 do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @base opts[:base]
      @pool opts[:pool]
      @table opts[:table]
      @mod opts[:mod]
      @create opts[:create]
      @alter opts[:alter] || []
      @attach opts[:attach] || []
      @version opts[:version] || 0
      @keys opts[:keys] || 1
      @cache opts[:cache] || false
      @max_cache_size opts[:cache_size] || 10_000_000
      @stmts opts[:stmt]

      require Logger
      alias Exqlite.Sqlite3NIF
      alias Exqlite.Sqlite3
      alias Ippan.Utils
      use GenServer

      def child_spec(opts) do
        state = start(opts)

        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [state]}
        }
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

      def start(path) when is_binary(path) do
        {:ok, conn} = open(path)

        start(conn)
      end

      def start(conn) when is_reference(conn) do
        :ok = create(conn)

        statements =
          for {name, sql} <- @stmts, into: %{} do
            {:ok, statement} = Sqlite3NIF.prepare(conn, sql)
            {name, statement}
          end

        check_version(conn)
        attach(conn)

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
        Sqlite3NIF.execute(conn, ~c"PRAGMA journal_mode = WAL")
        Sqlite3NIF.execute(conn, ~c"PRAGMA synchronous = 0")
        Sqlite3NIF.execute(conn, ~c"PRAGMA cache_size = 1000000")
        Sqlite3NIF.execute(conn, ~c"PRAGMA temp_store = memory")
        Sqlite3NIF.execute(conn, ~c"PRAGMA mmap_size = 30000000000")
        Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
        # Sqlite3NIF.execute(conn, 'PRAGMA threads = #{:erlang.system_info(:schedulers_online)}')
        # Sqlite3NIF.execute(conn, 'PRAGMA locking_mode = EXCLUSIVE')
        # Sqlite3NIF.execute(conn, 'PRAGMA read_uncommitted = true')
        # Sqlite3NIF.execute(conn, 'PRAGMA page_size = 32768')
        {:ok, conn}
      end

      # @spec open_readonly(binary()) :: {:ok, term()}
      # def open_readonly(path) do
      #   flags = [:sqlite_open_readonly, :sqlite_open_uri]

      #   result = {:ok, conn} = Sqlite3.open(path, flags)
      #   Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
      #   result
      # end

      # def open_memory do
      #   result = {:ok, conn} = Sqlite3.open(":memory:")
      #   Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
      #   result
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
        cast({:step, :insert, params})
      end

      @spec insert_sync(list()) :: integer() | :busy | {:error, term()}
      def insert_sync(params) do
        call({:step, :insert, params})
      end

      def one(key) when is_list(key) do
        case call({:step, :lookup, key}) do
          {:row, []} -> nil
          {:row, data} -> data
          _ -> nil
        end
      end

      def one(key) do
        case call({:step, :lookup, [key]}) do
          {:row, []} -> nil
          {:row, data} -> data
          _ -> nil
        end
      end

      def exists?(key) do
        {:row, [1]} == call({:step, :exists, [key]})
      end

      def owner?(key, owner) do
        {:row, [1]} == call({:step, "owner", [key, owner]})
      end

      @spec all() :: {:ok, [any()]} | {:error, term()}
      def all do
        call({:execute, :fetch, ~c"SELECT * FROM #{@table}", []})
      end

      def delete(params) do
        call({:step, :delete, params})
      end

      @spec update(map() | Keyword.t(), Keyword.t()) :: non_neg_integer() | {:error, term()}
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

        call({:update, set_fields, where, values ++ w_values})
      end

      def delete_all do
        call({:execute, ~c"DELETE FROM #{@table}"})
      end

      def step(name, params) do
        call({:step, name, params})
      end

      def fetch(name, params) do
        call({:fetch, name, params})
      end

      def step_change(name, params) do
        call({:changes, name, params})
      end

      def changes(conn) do
        Sqlite3NIF.changes(conn)
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
        call({:execute, ~c"PRAGMA wal_checkpoint(TRUNCATE)"})
      end

      def drop do
        call({:execute, ~c"DROP TABLE #{@table}"})
      end

      @impl true
      def handle_cast(
            {:step, stmt_name, params},
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        Sqlite3NIF.bind_and_step(conn, statement, params)
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
        {:reply, Sqlite3NIF.bind_and_step(conn, statement, params), state}
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
            {:changes, stmt_name, params},
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

      def handle_call({:execute, operator, query, args}, _from, %{conn: conn, stmt: stmt} = state) do
        {:ok, statement} = Sqlite3NIF.prepare(conn, query)

        result =
          case operator do
            :step ->
              Sqlite3NIF.bind_and_step(conn, statement, args)

            :fetch ->
              if args != [] do
                Sqlite3NIF.bind(conn, statement, args)
              end

              Sqlite3.fetch_all(conn, statement)

            :change ->
              Sqlite3NIF.bind_step_changes(conn, statement, args)
          end

        Sqlite3NIF.release(conn, statement)
        {:reply, result, state}
      end

      def handle_call({:call, fun}, _from, state) do
        try do
          fun.(state)
        rescue
          _ex ->
            {:reply, :error, state}
        end
      end

      def handle_call(
            {:update, set_fields, where, values_list},
            _from,
            %{conn: conn} = state
          ) do
        {:ok, statement} =
          Sqlite3NIF.prepare(conn, ~c"UPDATE #{@table} SET #{set_fields} WHERE #{where}")

        n = Sqlite3NIF.bind_step_changes(conn, statement, values_list)
        Sqlite3NIF.release(conn, statement)

        {:reply, n, state}
      end

      def handle_call(:sync, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")
        Sqlite3NIF.execute(conn, ~c"BEGIN")
        {:reply, :ok, state}
      end

      @impl true
      def terminate(_reason, %{conn: conn, stmt: stmts} = state) do
        stop(conn, stmts)
      end

      def stop(conn, stmts) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")
        Sqlite3NIF.execute(conn, ~c"VACUUM")
        Sqlite3NIF.execute(conn, ~c"PRAGMA optimize")

        for stmt <- stmts do
          Sqlite3NIF.release(conn, stmt)
        end

        Sqlite3NIF.close(conn)
      end

      defoverridable init: 1,
                     stop: 2,
                     one: 1,
                     all: 0,
                     insert: 1,
                     insert_sync: 1,
                     update: 2,
                     delete: 1,
                     delete_all: 0,
                     exists?: 1,
                     owner?: 2,
                     terminate: 2
    end
  end
end
