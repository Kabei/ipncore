defmodule Ippan.Struct do
  defmacro __using__(_opts) do
    quote location: :keep do
      @callback edit_fields() :: [atom()]
      @callback to_list(map()) :: list()
      @callback to_tuple(map()) :: tuple()
      @callback to_map(tuple() | list()) :: map()

      @spec fields :: [atom()]
      def fields do
        __MODULE__.__struct__() |> Map.keys()
      end
    end
  end
end
