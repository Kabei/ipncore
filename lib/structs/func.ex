defmodule Ippan.Func do
  @moduledoc """
  * Priority
     0. Maximum
     1. Very high
     2. High
     3. Normal
     4. Low
     5. Minimum
     d. deferred
  """
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          name: String.t(),
          mod: module(),
          modx: module(),
          fun: atom() | function(),
          index: term(),
          # parallel: boolean(),
          # deferred: boolean(),
          priority: non_neg_integer() | binary(),
          # unique: boolean(),
          origin: integer(),
          flag: integer()
        }

  @enforce_keys [:id, :mod, :modx, :fun]
  defstruct [
    :id,
    :name,
    :mod,
    :modx,
    :fun,
    # deferred: false,
    # parallel: true,
    # unique: false,
    origin: 0,
    priority: 3,
    index: 2,
    flag: 0
  ]
end
