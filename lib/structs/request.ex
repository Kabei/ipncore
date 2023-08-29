defmodule Ippan.Request do
  @type t :: %__MODULE__{
          type: non_neg_integer(),
          timestamp: non_neg_integer(),
          from: binary(),
          args: list(),
          signature: binary() | nil
        }

  @type result :: :ok | {:ok, term()} | {:notify, term()} | :error | {:error, term()}

  defstruct type: 0, timestamp: 0, from: nil, args: [], signature: nil
end
