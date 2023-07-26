defmodule Store.Cache do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @table opts[:table]
      @mod opts[:mod]
      @partial opts[:mode] == "partial"
      @full opts[:mode] == "full"
      @size opts[:size] || 0
      @keypos opts[:keypos] || 1
      @opts [
        :set,
        :named_table,
        :public,
        read_concurrency: true,
        write_concurrency: true
      ]

      alias Ippan.Utils

      if @full do
        def init(state) do
          :ets.new(@table, @opts)

          for item <- all() do
            :ets.insert(@table, @mod.to_tuple(item))
          end

          begin(state.conn)

          {:ok, state}
        end

        def exists?(key) do
          :ets.member(@table, key)
        end

        def get(key, default \\ nil) do
          case :ets.lookup(@table, key) do
            [] ->
              default

            [{_k, val} | _] ->
              val
          end
        end

        def put(key, value) do
          :ets.insert(@table, {key, value})
          cast({:step, :insert, [key, value]})
        end
      end

      if @partial do
        def init(state) do
          :ets.new(@table, @opts)

          begin(state.conn)

          {:ok, state}
        end

        def lookup(key) do
          case :ets.lookup(@table, key) do
            [] ->
              case one(key) do
                nil ->
                  nil

                data ->
                  :ets.insert(@table, @mod.to_tuple(data))

                  if :ets.info(@table, :size) > @size do
                    :ets.delete_all_objects(@table)
                  end

                  @mod.to_tuple(data)
              end

            [val] ->
              val
          end
        end

        def lookup_list(key) do
          case :ets.lookup(@table, key) do
            [] ->
              case one(key) do
                nil ->
                  nil

                data ->
                  :ets.insert(@table, @mod.to_tuple(data))

                  if :ets.info(@table, :size) > @size do
                    :ets.delete_all_objects(@table)
                  end

                  data
              end

            [val] ->
              @mod.to_list(val)
          end
        end

        def lookup_map(key) do
          case :ets.lookup(@table, key) do
            [] ->
              case one(key) do
                nil ->
                  nil

                data ->
                  :ets.insert(@table, @mod.to_tuple(data))

                  if :ets.info(@table, :size) > @size do
                    :ets.delete_all_objects(@table)
                  end

                  @mod.to_map(data)
              end

            [val] ->
              @mod.to_map(val)
          end
        end

        def delete(key) do
          :ets.delete(@table, key)
          call({:step, :delete, [key]})
        end
      end

      def update(map_set_fields, map_where) do
        [{_key, val} | _] = map_where

        :ets.delete(@table, val)

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

      def terminate(_reason, %{conn: conn, stmt: stmts} = state) do
        stop(conn, stmts)
        :ets.delete(@table)
      end

      defp get_key([key]), do: key
      defp get_key(params) when is_list(params), do: Enum.take(params, @keypos)
      defp get_key(key), do: key
    end
  end
end
