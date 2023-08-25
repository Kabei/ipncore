defmodule Ippan.Request.Source do
  alias Ippan.Event

  @type t :: %__MODULE__{
          hash: binary(),
          conn: reference(),
          dets: pid(),
          stmts: map(),
          event: Event.t(),
          id: String.t() | nil,
          validator: number() | nil,
          node: number() | nil,
          timestamp: non_neg_integer(),
          size: non_neg_integer()
        }
  defstruct [:hash, :conn, :dets, :stmts, :event, :id, :validator, :node, :timestamp, :size]
end
