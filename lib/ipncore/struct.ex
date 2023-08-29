defmodule Ippan.Struct do
  @callback to_list(map()) :: list()
  @callback to_map(tuple()) :: map()
  @callback to_tuple(map()) :: tuple()
  @callback list_to_map(list()) :: map()
  @callback list_to_tuple(list()) :: tuple()

  @callback optionals() :: [binary()]
  @callback editable() :: [binary()]

  @optional_callbacks [optionals: 0, editable: 0]

  # defmacro __using__(_opts) do
  #   quote location: :keep do
  #     @behaviour Ippan.Struct

  #     @callback edit_fields() :: [atom()]

  #     @spec fields :: [atom()]
  #     def fields do
  #       __MODULE__.__struct__() |> Map.keys()
  #     end
  #   end
  # end
end
