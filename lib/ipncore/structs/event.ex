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
          parallel: boolean(),
          system: boolean(),
          auth: boolean(),
          before: atom() | function(),
          after: atom() | function()
        }

  @enforce_keys [:id, :name, :base, :mod, :fun, :parallel, :auth]
  defstruct [:id, :name, :base, :mod, :fun, :parallel, :after, :before, auth: true, system: false]
end
