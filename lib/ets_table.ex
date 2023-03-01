defmodule ETSTable do
  defmacro __using__(opts) do
    quote location: :keep do
      @opts unquote(opts)
      @end_of_table :"$end_of_table"
      @table Keyword.get(@opts, :name) || throw("no table name")

      @ets_opts Keyword.get(@opts, :ets_opts, [
                  :set,
                  :public,
                  :named_table,
                  read_concurrency: true,
                  write_concurrency: true
                ])

      def open do
        :ets.whereis(@table)
        |> case do
          :undefined ->
            :ets.new(@table, @ets_opts)

          _ ->
            :ok
        end
      end

      def insert(x), do: :ets.insert(@table, x)
      def insert_new(x), do: :ets.insert_new(@table, x)

      def lookup(key) do
        case :ets.lookup(@table, key) do
          [obj] -> obj
          _ -> nil
        end
      end

      def keys do
        Stream.resource(
          fn -> :ets.first(@table) end,
          fn
            @end_of_table -> {:halt, nil}
            previous_key -> {[previous_key], :ets.next(@table, previous_key)}
          end,
          fn _ -> :ok end
        )
      end

      def tab2list, do: :ets.tab2list(@table)

      def select(fun) do
        :ets.select(@table, fun)
      end

      def select_delete(fun) do
        :ets.select_delete(@table, fun)
      end

      def size, do: :ets.info(@table, :size)

      def first do
        case :ets.first(@table) do
          @end_of_table ->
            nil

          key ->
            case :ets.lookup(@table, key) do
              [val] ->
                val

              _ ->
                nil
            end
        end
      end

      def last do
        case :ets.last(@table) do
          @end_of_table ->
            nil

          key ->
            case :ets.lookup(@table, key) do
              [val] ->
                val

              _ ->
                nil
            end
        end
      end

      def delete(key) do
        :ets.delete(@table, key)
      end
    end
  end
end
