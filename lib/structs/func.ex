defmodule Ippan.Func do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          name: String.t(),
          mod: module(),
          fun: atom() | function(),
          deferred: boolean(),
          check: integer()
        }

  @enforce_keys [:id, :mod, :fun]
  defstruct [
    :id,
    :name,
    :mod,
    :fun,
    deferred: false,
    check: 1
  ]
end
