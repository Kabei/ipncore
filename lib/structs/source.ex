defmodule Ippan.Request.Source do
  alias Ippan.{Account, Event}

  @type t :: %__MODULE__{
          hash: binary(),
          event: Event.t(),
          account: Account.t() | nil,
          timestamp: non_neg_integer(),
          size: non_neg_integer()
        }
  defstruct [:hash, :event, :account, :timestamp, :size]
end
