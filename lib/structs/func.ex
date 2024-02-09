defmodule Ippan.Func do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          name: String.t(),
          mod: module(),
          modx: module(),
          fun: atom() | function(),
          deferred: boolean(),
          check: integer(),
          key: integer() | nil
        }

  @enforce_keys [:id, :mod, :modx, :fun]
  defstruct [
    :id,
    :name,
    :mod,
    :modx,
    :fun,
    :key,
    deferred: false,
    check: 0
  ]
end
