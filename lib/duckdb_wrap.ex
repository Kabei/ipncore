defmodule Ippan.Store.DuckDBWrap do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @behaviour Ippan.Store.Relational

      @base opts[:base]
      @mem opts[:mem]
      @app opts[:app]
      @filename opts[:filename]
      @folder opts[:folder]
      @table opts[:table]
      @sql opts[:sql]
      @threads opts[:threads] || System.schedulers()

      use GenServer

      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :supervisor
        }
      end

      def start_link(opts) do
        case GenServer.whereis(@base) do
          nil ->
            {otp_app, folder_atom} = @folder
            folder_path = Application.get_env(otp_app, folder_atom)
            File.mkdir_p(folder_path)
            path = Path.join(folder_path, @filename)

            GenServer.start_link(__MODULE__, %{path: path}, name: @base)

          {_atom, _node} ->
            raise MatchError, "Bad return GenServer whereis"

          pid ->
            :binary.bin_to_list()

            GenServer.call(pid, {:sql, @table, @sql})
            {:ok, pid}
        end
      end

      @impl true
      def init(opts) do
        {:ok, db} = Duckdbex.open(opts.path, %Duckdbex.Config{checkpoint_wal_size: 1_000_000_000})
        {:ok, conn} = Duckdbex.connection(db)

        create()

        {:ok, %{db: db, conn: conn, sql: @sql}}
      end

      def base, do: @base

      def create do
        case execute_stmt(:create, [], false) do
          {:ok, _} ->
            :ok

          error ->
            error
        end
      end

      def drop do
        %{conn: conn} = :sys.get_state(@base)

        case Duckdbex.query(conn, "DROP TABLE IF EXISTS #{@table}") do
          {:ok, _} ->
            :ok

          error ->
            error
        end
      end

      def insert(params) do
        case execute_stmt(:insert, params, false) do
          {:ok, _} ->
            :ok

          error ->
            error
        end
      end

      def insert_new(params) do
        case execute_stmt(:insert_new, params, false) do
          {:ok, _} ->
            true

          _error ->
            false
        end
      end

      def insert_all(list) do
        %{conn: conn} = :sys.get_state(@base)

        case Duckdbex.appender(conn, @table) do
          {:ok, appender} ->
            is_multi_list = List.first(list) |> is_list()

            if is_multi_list do
              Duckdbex.appender_add_rows(appender, list)
            else
              for x <- list do
                Duckdbex.appender_add_row(appender, x)
              end
            end

            Duckdbex.appender_close(appender)

          error ->
            error
        end
      end

      def exists?(key) do
        case execute_stmt(:exists, [key], true) do
          [1] ->
            true

          _ ->
            false
        end
      end

      def not_exists?(key) do
        case execute_stmt(:exists, [key], true) do
          [1] ->
            true

          _ ->
            false
        end
      end

      def get(key) do
        case execute_stmt(:select, [key], true) do
          {:error, _} ->
            nil

          [] ->
            nil

          [result] ->
            result
        end
      end

      def keys do
        case execute_stmt(:keys, [], true) do
          results when is_list(results) ->
            results

          _ ->
            []
        end
      end

      def all do
        case execute_stmt(:all, [], true) do
          {:error, _} ->
            []

          [] ->
            []

          results ->
            results
        end
      end

      def owner?(key, owner) do
        case execute_stmt(:exists_owner, [key, owner], true) do
          [1] ->
            true

          _ ->
            false
        end
      end

      def get_if_owner(key, owner) do
        case execute_stmt(:select_owner, [key, owner], true) do
          [result] ->
            result

          _ ->
            nil
        end
      end

      def delete(key) do
        case execute_stmt(:delete, [key], true) do
          {:error, _} = error ->
            error

          x when is_list(x) ->
            :ok
        end
      end

      def delete_all do
        case execute_stmt(:delete_all, [], true) do
          {:error, _} ->
            0

          [0] ->
            0

          list ->
            length(list)
        end
      end

      def count do
        case execute_stmt(:count, [], true) do
          [n] ->
            n

          _ ->
            0
        end
      end

      def start_appender do
        %{conn: conn} = :sys.get_state(@base)

        case Duckdbex.appender(conn, @table) do
          {:ok, appender} ->
            {:ok, conn, appender}

          error ->
            error
        end
      end

      def update(where, props) do
        %{conn: conn} = :sys.get_state(@base)

        where_sql = normalize_params_set(where)

        set_sql = normalize_params_set(props)

        sql = "UPDATE #{@table} SET #{set_sql} WHERE #{where_sql}"

        case Duckdbex.query(conn, "DROP TABLE IF EXISTS #{@table}") do
          {:ok, _} ->
            :ok

          error ->
            error
        end
      end

      def sync do
        %{mem: mem} = :sys.get_state(@base)

        stmt_ref = Duckdbex.prepare_statement(mem, sql[@table][:all])

        case Duckdbex.execute_statement(stmt_ref, params) do
          {:ok, res} ->
            case Duckdbex.fetch_chunk(res) do
              [] ->
                :ok

              results ->
                {:ok, conn, appender} = start_append()
                Duckdbex.appender_add_rows(appender, results)
                Duckdbex.appender_close(appender)

                with {:ok, _res} <- Duckdbex.query(mem, sql[@table][:delete_all]) do
                  :ok
                else
                  :error
                end
            end

          _error ->
            :error
        end
      end

      defp normalize_params_set(kw) do
        for {k, v} <- kw do
          "#{k}=#{normalize(v)}"
        end
        |> Enum.join(" ")
        |> String.replace("\"", "'")
      end

      defp normalize(v) when is_tuple(v) do
        Tuple.to_list(v)
        |> Jason.encode!()
      end

      defp normalize(v) when is_map(v) or is_list(v) do
        Jason.encode!(v)
      end

      defp normalize(v), do: v

      defp memory_execute_stmt(stmt_name, params, true) when is_reference(conn) do
        %{mstmt: mstmt} = :sys.get_state(@base)
        stmt_ref = mstmt[@table][stmt_name]

        case Duckdbex.execute_statement(stmt_ref, params) do
          {:ok, res} ->
            Duckdbex.fetch_chunk(res)

          error ->
            error
        end
      end

      defp execute_stmt(stmt_name, params, false) do
        %{sql: sql} = :sys.get_state(@base)
        stmt_ref = sql[@table][stmt_name]
        Duckdbex.execute_statement(stmt_ref, params)
      end

      defp execute_stmt(stmt_name, params, true) do
        %{sql: sql} = :sys.get_state(@base)
        stmt_ref = sql[@table][stmt_name]

        case Duckdbex.execute_statement(stmt_ref, params) do
          {:ok, res} ->
            Duckdbex.fetch_chunk(res)

          error ->
            error
        end
      end

      @impl true
      def handle_call({:state, new_state}, _from, _state) do
        {:reply, :ok, new_state}
      end

      def handle_call({:sql, table, sql}, _from, %{sql: old_sql} = state) do
        new_sql = %{table => sql}
        merge = Map.merge(old_sql, new_sql)
        {:reply, :ok, %{state | sql: merge}}
      end
    end
  end
end
