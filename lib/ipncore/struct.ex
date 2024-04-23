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

# defmodule Ippan.Struct do
#   # @callback to_list(map()) :: list()
#   # @callback to_map(tuple()) :: map()
#   # @callback to_tuple(map()) :: tuple()
#   # @callback list_to_map(list()) :: map()
#   # @callback list_to_tuple(list()) :: tuple()

#   @callback props() :: list()
#   @callback optionals() :: [binary()]
#   @callback editable() :: [binary()]

#   @callback put(reference(), map()) :: term()
#   @callback get(reference(), term()) :: map() | nil
#   @callback has?(reference(), term()) :: boolean()
#   @callback update(reference(), term(), binary()) :: term()
#   @callback del(reference(), term()) :: term()

#   @callback as_maps(term, term) :: [map()]
#   @callback as_map(term, term) :: map()

#   @optional_callbacks [optionals: 0, editable: 0]

#   def as_maps(rows, columns) do
#     Enum.map(rows, fn row ->
#       :lists.zip(columns, row) |> :maps.from_list()
#     end)
#   end

#   def as_map(columns, row) do
#     :lists.zip(columns, row) |> :maps.from_list()
#   end

#   defmacro __using__(_opts) do
#     quote location: :keep do
#       # @behaviour Ippan.Struct

#       @compile {:inline, [as_map: 1, as_map: 2, as_list: 1]}

#       @spec fields :: [atom()]
#       def fields do
#         __MODULE__.__struct__() |> Map.delete(:__struct__) |> Map.keys()
#       end

#       def as_maps(rows) do
#         columns = fields()

#         Enum.map(rows, fn row ->
#           as_map(columns, row)
#         end)
#       end

#       def as_map(row) do
#         list = :lists.zip(fields(), row)
#         struct(__MODULE__, list)
#       end

#       def as_map(columns, row) do
#         list = :lists.zip(columns, row)
#         struct(__MODULE__, list)
#       end

#       def as_list(map) do
#         Enum.unzip(map)
#       end
#     end
#   end
# end
