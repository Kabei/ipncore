defmodule Ippan.Request.Source do
  alias Ippan.Event

  @type t :: %__MODULE__{
          hash: binary(),
          event: Event.t(),
          id: String.t() | nil,
          validator: number() | nil,
          timestamp: non_neg_integer(),
          size: non_neg_integer()
        }
  defstruct [:hash, :event, :id, :validator, :timestamp, :size]
end
