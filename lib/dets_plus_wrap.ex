defmodule Ippan.Store.DetsPlusWrap do
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts], location: :keep do
      @behaviour Ippan.Store.KV

      @base opts[:base]
      @filename opts[:filename]
      @otp_app opts[:otp_app]
      @folder opts[:folder]
      @keypos opts[:keypos] || 1

      def child_spec(opts) do
        %{
          id: __MODULE__,
          start: {__MODULE__, :start_link, [opts]},
          type: :supervisor
        }
      end

      def start_link(opts) do
        folder_path = Application.get_env(@otp_app, @folder)
        File.mkdir_p(folder_path)
        path = Path.join(folder_path, @filename)

        opts =
          Keyword.merge(
            [file: path, keypos: @keypos, auto_save: 5_000, auto_save_memory: 1_000_000],
            opts
          )

        {:ok, result} = DetsPlus.open_file(@base, opts)
        {:ok, result.pid}
      end

      def base do
        @base
      end

      def insert(x) do
        DetsPlus.insert(@base, x)
      end

      def insert_new(x) do
        DetsPlus.insert_new(@base, x)
      end

      def exists?(key) do
        DetsPlus.member(@base, key)
      end

      def not_exists?(key) do
        not DetsPlus.member(@base, key)
      end

      def keys do
        DetsPlus.reduce(@base, [], fn {key, _}, acc ->
          acc ++ [key]
        end)
      end

      def get(key) do
        DetsPlus.lookup(@base, key)
      end

      def owner?(key) do
        case DetsPlus.lookup(@base, key) do
          nil -> false
          map -> map.owner == key
        end
      end

      def get_if_owner(key) do
        case DetsPlus.lookup(@base, key) do
          nil ->
            nil

          map ->
            if map.owner == key do
              map
            else
              nil
            end
        end
      end

      def delete(key) do
        DetsPlus.delete(@base, key)
      end

      def delete_all do
        DetsPlus.delete_all_objects(@base)
      end

      def info do
        DetsPlus.info(@base)
      end

      def size do
        DetsPlus.info(@base, :size)
      end

      def file_size do
        DetsPlus.info(@base, :file_size)
      end
    end
  end
end
