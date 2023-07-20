defmodule Ippan.Struct do
  @callback to_list(map()) :: list()
  @callback to_tuple(map()) :: tuple()
  @callback to_map(tuple() | list()) :: map()

  # @callback to_ets(list() | map()) :: tuple()
  # @callback from_ets(tuple()) :: map()

  defmacro __using__(_opts) do
    quote location: :keep do
      @behaviour Ippan.Struct

      @callback edit_fields() :: [atom()]

      @spec fields :: [atom()]
      def fields do
        __MODULE__.__struct__() |> Map.keys()
      end
    end
  end
end
