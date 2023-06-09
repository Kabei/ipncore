# auth_type
# 0. None
# 1. Falcon-512
# 2. Hash entangled signature: Sha3-256 and poly-1305/blake3
defmodule Ippan.Event do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          name: String.t(),
          base: atom(),
          mod: module(),
          fun: atom() | function(),
          deferred: boolean(),
          system: boolean(),
          auth: boolean(),
          validator: boolean(),
          before: atom() | function(),
          after: atom() | function()
        }

  @enforce_keys [:id, :name, :base, :mod, :fun]
  defstruct [
    :id,
    :name,
    :base,
    :mod,
    :fun,
    :after,
    :before,
    deferred: false,
    validator: true,
    auth: true,
    system: false
  ]
end
