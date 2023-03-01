defmodule DBTable do
  @moduledoc """

  """

  defmacro __using__(opts) do
    quote location: :keep do
      @opts unquote(opts)
      @name Keyword.get(@opts, :name)
      @path Keyword.get(@opts, :path)
      @filename Keyword.get(@opts, :filename, "")
      @auto_save Keyword.get(@opts, :auto_save, 5_000)
      @shards Keyword.get(@opts, :shards, 1)
      @extension Keyword.get(@opts, :ext, ".db")
      @keypos Keyword.get(@opts, :keypos, 0)
      @auto_save_memory Keyword.get(@opts, :auto_save_memory, 1_000_000)
      @page_cache_memory Keyword.get(@opts, :page_cache_memory, 1_000_000_000)
      @range_of_shards 0..(@shards - 1)
      # cache = Keyword.get(@opts, :cache, true)

      def root_path do
        Application.get_env(:ipncore, :database_path, "data")
      end

      def open do
        directory_path = root_path() |> Path.join(@path)
        File.mkdir_p(directory_path)

        for shard <- @range_of_shards do
          pid = pid(shard)

          absolute_path = absolute_path(directory_path, shard)

          DetsPlus.open_file(id,
            file: absolute_path,
            auto_save: @auto_save,
            auto_save_memory: @auto_save_memory,
            page_cache_memory: @page_cache_memory
          )
        end
      end

      def close do
        for shard <- @range_of_shards do
          pid(shard)
          |> DetsPlus.close()
        end
      end

      defp absolute_path(directory_path, shard) do
        Path.join(
          directory_path,
          IO.iodata_to_binary([@filename, to_string(shard), @extension])
        )
      end

      # CRUD

      def put(x) when is_tuple(x) do
        key = elem(x, @keypos)
        pid = pid_from_key(key)
        DetsPlus.insert(pid, x)
      end

      def put(x) do
        key = Map.fetch!(x, @keypos)
        pid = pid_from_key(key)
        DetsPlus.insert(pid, x)
      end

      def put(key, x) do
        pid = pid_from_key(key)
        DetsPlus.insert(pid, x)
      end

      def put_new(x) do
        key = Map.fetch!(x, @keypos)
        pid = pid_from_key(key)
        DetsPlus.insert_new(base, {hash, pubkey})
      end

      def put_new!(x) do
        key = Map.fetch!(x, @keypos)
        pid = pid_from_key(key)

        DetsPlus.insert_new(pid, x) || raise RuntimeError, message: "already exists"
      end

      def fetch(key) do
        pid = pid_from_key(address_hash)

        case DetsPlus.lookup(pid, address_hash) do
          [x] -> x
          _ -> nil
        end
      end

      def fetch!(key) do
        pid = pid_from_key(address_hash)

        case DetsPlus.lookup(pid, address_hash) do
          [x] ->
            x

          _ ->
            raise RuntimeError, message: "resource not exists"
        end
      end

      def fetch_owner!(key, owner) do
        pid = pid_from_key(address_hash)

        case DetsPlus.lookup(pid, host) do
          [x] when x.owner == owner ->
            x

          [_x] ->
            raise RuntimeError, "invalid owner"

          _ ->
            raise RuntimeError, "resource not exists"
        end
      end

      def exists?(key) do
        pid = pid_from_key(key)
        DetsPlus.member?(pid, key)
      end

      def exists!(key) do
        pid = pid_from_key(key)

        case DetsPlus.lookup(pid, x) do
          [] ->
            false

          _ ->
            raise RuntimeError, "already exists"
        end
      end

      def delete(key) do
        pid = pid_from_key(key)
        DetsPlus.delete(pid, key)
      end

      def delete_owner!(key, owner) do
        pid = pid_from_key(key)

        case DetsPlus.lookup(pid, key) do
          [x] when x.owner == owner ->
            case DetsPlus.delete(pid, key) do
              {:error, _} -> raise RuntimeError, "error in the operation"
              r -> r
            end

          [_x] ->
            raise RuntimeError, "invalid owner"

          _ ->
            raise RuntimeError, "not exists"
        end
      end

      # utils
      defp fetch_key(x) when is_tuple(x) do
        elem(x, keypos)
      end

      defp fetch_key(x) do
        Map.fetch!(x, @keypos)
      end

      defp pid_from_key(key) when is_binary(key) do
        first = :binary.first(key)
        number = rem(first, @shards)

        [name, to_string(number)]
        |> IO.iodata_to_binary()
        |> String.to_existing_atom()
      end

      defp pid_from_key(key) when is_integer(key) do
        number = rem(key, @shards)

        [name, to_string(number)]
        |> IO.iodata_to_binary()
        |> String.to_existing_atom()
      end

      defp pid(shard) do
        number = rem(shard, @shards)

        [name, to_string(number)]
        |> IO.iodata_to_binary()
        |> String.to_existing_atom()
      end
    end
  end
end
