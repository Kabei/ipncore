defmodule Store.Sqlite do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @base opts[:base]
      @pool opts[:pool]
      @table opts[:table]
      @mod opts[:mod]
      @create opts[:create]
      @alter opts[:alter] || []
      @version opts[:alter] || 0
      @keys opts[:keys] || 1
      @cache opts[:cache] || false
      @max_cache_size opts[:cache_size] || 10_000_000
      @stmts opts[:stmt]

      require Logger
      alias Exqlite.Sqlite3NIF
      alias Exqlite.Sqlite3
      alias Ippan.Utils
      use GenServer

      if @pool do
        def child_spec(path) do
          {:ok, state} = start(path)

          :poolboy.child_spec(
            @pool,
            [
              name: {:local, @pool},
              worker_module: __MODULE__,
              size: :erlang.system_info(:schedulers_online),
              max_overflow: 0
            ],
            state
          )
        end
      else
        def child_spec(opts) do
          {:ok, state} = start(opts)

          %{
            id: __MODULE__,
            start: {__MODULE__, :start_link, [state]}
          }
        end
      end

      if @pool do
        def start_link(path) do
          {:ok, state} = start(path)
          GenServer.start_link(__MODULE__, state, hibernate_after: 5_000)
        end
      else
        def start_link(path) when is_binary(path) do
          {:ok, state} = start(path)
          GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: @base)
        end

        def start_link(state) do
          GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: @base)
        end
      end

      if @pool do
        defmacrop call(request, timeout \\ :infinity) do
          quote do
            :poolboy.transaction(
              @pool,
              fn pid ->
                :gen_server.call(pid, unquote(request), unquote(timeout))
              end,
              unquote(timeout)
            )
          end
        end
      else
        defmacrop call(request, timeout \\ :infinity) do
          quote do
            :gen_server.call(@base, unquote(request), unquote(timeout))
          end
        end
      end

      if @pool do
        defmacrop cast(request) do
          quote do
            :poolboy.transaction(
              @pool,
              fn pid ->
                :gen_server.cast(pid, unquote(request))
              end,
              :infinity
            )
          end
        end
      else
        defmacrop cast(request) do
          quote do
            :gen_server.cast(@base, unquote(request))
          end
        end
      end

      defp start(path) when is_binary(path) do
        {:ok, conn} = open(path)

        start(conn)
      end

      defp start(conn) when is_reference(conn) do
        :ok = create(conn)

        statements =
          for {name, sql} <- @stmts, into: %{} do
            {:ok, statement} = Sqlite3.prepare(conn, sql)
            {name, statement}
          end

        check_version(conn)
        begin(conn)

        if @cache do
          {:ok, %{conn: conn, stmt: statements, ets: :ets.new(@base, [:set, :public])}}
        else
          {:ok, %{conn: conn, stmt: statements}}
        end
      end

      @impl true
      def init(state) do
        {:ok, state}
      end

      @spec open(binary()) :: {:ok, reference()}
      def open(path) do
        path
        |> Path.dirname()
        |> File.mkdir_p()

        # flags = [:sqlite_open_sharedcache]
        flags = []

        {:ok, conn} = Sqlite3.open(path, flags)
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

      @spec open_readonly(binary()) :: {:ok, term()}
      def open_readonly(path) do
        flags = [:sqlite_open_readonly, :sqlite_open_uri]

        result = {:ok, conn} = Sqlite3.open(path, flags)
        Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
        result
      end

      def open_memory do
        result = {:ok, conn} = Sqlite3.open(":memory:")
        Sqlite3NIF.execute(conn, ~c"PRAGMA case_sensitive_like = ON")
        result
      end

      def begin do
        call(:begin)
      end

      def commit do
        call(:commit)
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
          Sqlite3NIF.execute(conn, String.to_charlist(sql))
        end

        :ok
      end

      def insert(params) do
        cast({:insert, params})
      end

      @spec insert_sync(list()) :: integer() | :busy | {:error, term()}
      def insert_sync(params) do
        call({:insert, params})
      end

      def upsert(params) do
        cast({:upsert, params})
      end

      def replace(params) do
        cast({:replace, params})
      end

      def lookup(key) do
        call({:lookup, key})
      end

      def exists?(key) do
        call({:exists, key})
      end

      def owner?(key, owner) do
        call({:owner, key, owner})
      end

      @spec all() :: {:ok, [any()]} | {:error, term()}
      def all do
        call(:all)
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

      @spec update(map | Keyword.t(), map | Keyword.t()) ::
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

        call({:update, set_fields, where, values ++ w_values})
      end

      @spec delete(binary | list) :: non_neg_integer() | {:error, term()}
      def delete(key) do
        call({:delete, key})
      end

      def delete_all do
        call(:delete_all)
      end

      def sync do
        call(:sync)
      end

      def checkpoint do
        call(:checkpoint)
      end

      # def move_deferred(round) do
      #   call({:move_deferred, round})
      # end

      def drop do
        call(:drop)
      end

      def execute_fetch(stmt_name, params) do
        call({:execute_fetch, stmt_name, params})
      end

      def execute_step(stmt_name, params) do
        call({:execute_step, stmt_name, params})
      end

      def execute_changes(stmt_name, params) do
        call({:execute_changes, stmt_name, params})
      end

      def execute(sql) do
        call({:execute, sql})
      end

      def launch(fun) do
        call({:call, fun})
      end

      if @cache do
        @impl true
        def handle_cast(
              {:insert, params},
              %{conn: conn, ets: ets, stmt: stmt} = state
            ) do
          statement = Map.get(stmt, :insert)
          r = Sqlite3NIF.bind_and_step(conn, statement, params)
          # :ets.delete(ets, params_to_ets(params))
          {:noreply, state}
        end

        @impl true
        def handle_call(
              {:lookup, params},
              _from,
              %{conn: conn, stmt: stmt, ets: ets} = state
            ) do
          ret =
            case :ets.lookup(ets, params_to_ets(params)) do
              [] ->
                case Sqlite3NIF.bind_and_step(conn, stmt.lookup, params) do
                  {:row, data} ->
                    if :ets.info(ets, :size) > @max_cache_size do
                      :ets.delete_all_objects(ets)
                    end

                    :ets.insert(ets, apply(@mod, :to_tuple, [data]))
                    lookup_transform(data)

                  error ->
                    nil
                end

              [value] ->
                lookup_transform(value)
            end

          {:reply, ret, state}
        end

        def handle_call(
              {:exists, id},
              _from,
              %{conn: conn, ets: ets, stmt: stmt} = state
            ) do
          case :ets.member(ets, id) do
            false ->
              statement = Map.get(stmt, :exists)
              result = {:row, [1]} == Sqlite3NIF.bind_and_step(conn, statement, [id])
              {:reply, result, state}

            true ->
              {:reply, true, state}
          end
        end

        def handle_call(
              {:owner, id, owner},
              _from,
              %{conn: conn, ets: ets, stmt: stmt} = state
            ) do
          case :ets.lookup(ets, id) do
            [] ->
              result = {:row, [1]} == Sqlite3NIF.bind_and_step(conn, stmt.owner, [id, owner])

              {:reply, result, state}

            [{_, value}] ->
              map = apply(@mod, :to_map, [value])
              {:reply, map.owner == owner, state}
          end
        end

        def handle_call(
              {:delete, params},
              _from,
              %{conn: conn, ets: ets, stmt: stmt} = state
            ) do
          statement = stmt.delete

          case Sqlite3NIF.bind_step_changes(conn, statement, params) do
            n when n > 0 ->
              :ets.delete(ets, params_to_ets(params))
              {:reply, n, state}

            _ ->
              {:reply, 0, state}
          end
        end

        def handle_call(:delete_all, _from, %{conn: conn, ets: ets} = state) do
          result = Sqlite3NIF.execute(conn, ~c"DELETE FROM #{@table}")
          :ets.delete_all_objects(ets)
          n = changes(conn)
          Sqlite3NIF.execute(conn, ~c"VACUUM")
          {:reply, n, state}
        end
      else
        @impl true
        def handle_cast({:insert, params}, %{conn: conn, stmt: stmt} = state) do
          Sqlite3NIF.bind_and_step(conn, stmt.insert, params)
          {:noreply, state}
        end

        def handle_cast({:upsert, params}, %{conn: conn, stmt: stmt} = state) do
          statement = Map.get(stmt, :upsert)
          Sqlite3NIF.bind_and_step(conn, statement, params)
          {:noreply, state}
        end

        def handle_cast({:replace, params}, %{conn: conn, stmt: stmt} = state) do
          statement = Map.get(stmt, :replace)
          Sqlite3NIF.bind_and_step(conn, statement, params)
          {:noreply, state}
        end

        @impl true
        def handle_call({:lookup, params}, _from, %{conn: conn, stmt: stmt} = state) do
          case Sqlite3NIF.bind_and_step(conn, stmt.lookup, params) do
            {:row, data} ->
              {:reply, lookup_transform(data), state}

            _ ->
              {:reply, nil, state}
          end
        end

        def handle_call({:exists, id}, _from, %{conn: conn, stmt: stmt} = state) do
          statement = Map.get(stmt, :exists)
          result = {:row, [1]} == Sqlite3NIF.bind_and_step(conn, statement, [id])
          {:reply, result, state}
        end

        def handle_call({:owner, id, owner}, _from, %{conn: conn, stmt: stmt} = state) do
          result = {:row, [1]} == Sqlite3NIF.bind_and_step(conn, stmt.owner, [id, owner])

          {:reply, result, state}
        end

        def handle_call({:delete, params}, _from, %{conn: conn, stmt: stmt} = state) do
          statement = stmt.delete

          case Sqlite3NIF.bind_step_changes(conn, statement, params) do
            n when n > 0 ->
              {:reply, n, state}

            _ ->
              {:reply, 0, state}
          end
        end

        def handle_call(:delete_all, _from, %{conn: conn} = state) do
          result = Sqlite3NIF.execute(conn, ~c"DELETE FROM #{@table}")
          n = changes(conn)
          Sqlite3NIF.execute(conn, ~c"VACUUM")
          {:reply, n, state}
        end
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
            {:insert, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        r = Sqlite3NIF.bind_step_changes(conn, stmt.insert, params)
        {:reply, r, state}
      end

      def handle_call(
            {:update, set_fields, where, values_list},
            _from,
            %{conn: conn} = state
          ) do
        {:ok, statement} =
          Sqlite3NIF.prepare(conn, ~c"UPDATE #{@table} SET #{set_fields} WHERE #{where}")

        case Sqlite3NIF.bind_step_changes(conn, statement, values_list) do
          n when n > 0 ->
            Sqlite3NIF.release(conn, statement)

            if @cache do
              case Map.get(state, :ets) do
                nil -> :ok
                ets -> :ets.delete(ets, params_to_ets(values_list))
              end
            end

            {:reply, n, state}

          _ ->
            Sqlite3NIF.release(conn, statement)
            {:reply, 0, state}
        end
      end

      def handle_call(:all, _from, %{conn: conn, stmt: stmt} = state) do
        {:ok, statement} = Sqlite3NIF.prepare(conn, ~c"SELECT * FROM #{@table}")
        result = Sqlite3.fetch_all(conn, statement)
        Sqlite3NIF.release(conn, statement)
        {:reply, result, state}
      end

      def handle_call(:sync, _from, %{conn: conn} = state) do
        commit(conn)
        begin(conn)
        {:reply, :ok, state}
      end

      def handle_call(:checkpoint, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, ~c"PRAGMA wal_checkpoint(TRUNCATE)")
        {:reply, :ok, state}
      end

      def handle_call(
            {:execute_step, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        {:reply, Sqlite3NIF.bind_and_step(conn, statement, params), state}
      end

      def handle_call(
            {:execute_fetch, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        Sqlite3NIF.bind(conn, statement, params)
        result = Sqlite3.fetch_all(conn, statement)
        {:reply, result, state}
      end

      def handle_call(
            {:execute_changes, stmt_name, params},
            _from,
            %{conn: conn, stmt: stmt} = state
          ) do
        statement = Map.get(stmt, stmt_name)
        {:reply, Sqlite3NIF.bind_step_changes(conn, statement, params), state}
      end

      def handle_call({:execute, sql}, _from, %{conn: conn} = state) do
        result = Sqlite3NIF.execute(conn, sql)
        {:reply, result, state}
      end

      def handle_call(:begin, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, ~c"BEGIN")
        {:reply, :ok, state}
      end

      def handle_call(:commit, _from, %{conn: conn} = state) do
        Sqlite3NIF.execute(conn, ~c"COMMIT")
        {:reply, :ok, state}
      end

      def handle_call(:create, _from, %{conn: conn} = state) do
        for sql <- List.wrap(@create) do
          Sqlite3NIF.execute(conn, String.to_charlist(sql))
        end

        {:reply, :ok, state}
      end

      def handle_call(:drop, _from, %{conn: conn} = state) do
        result = Sqlite3NIF.execute(conn, ~c"DROP TABLE #{@table}")
        Sqlite3NIF.execute(conn, ~c"VACUUM")
        {:reply, result, state}
      end

      @impl true
      def terminate(_reason, %{conn: conn, ets: ets} = state) do
        commit(conn)
        Sqlite3NIF.execute(conn, ~c"VACUUM")
        Sqlite3NIF.execute(conn, ~c"PRAGMA optimize")
        Sqlite3NIF.close(conn)
        :ets.delete(ets)
      end

      def terminate(_reason, %{conn: conn, stmt: stmts} = state) do
        commit(conn)
        Sqlite3NIF.execute(conn, ~c"VACUUM")
        Sqlite3NIF.execute(conn, ~c"PRAGMA optimize")

        for stmt <- stmts do
          Sqlite3NIF.release(conn, stmt)
        end

        Sqlite3NIF.close(conn)
      end

      def changes(conn) do
        {:ok, n} = Sqlite3NIF.changes(conn)
        n
      end

      # take a key value from params to ETS
      defp params_to_ets([key]), do: key
      defp params_to_ets(params) when is_list(params), do: Enum.take(params, @keys)
      defp params_to_ets(key), do: key

      if @mod do
        defp lookup_transform(x) do
          apply(@mod, :to_map, [x])
        end
      else
        defp lookup_transform(x), do: x
      end

      defoverridable init: 1,
                     all: 0,
                     terminate: 2,
                     lookup: 1,
                     insert: 1,
                     insert_sync: 1,
                     update: 2,
                     delete: 1,
                     exists?: 1,
                     owner?: 2
    end
  end
end
