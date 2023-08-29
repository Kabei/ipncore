defmodule Ippan.Balance do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: binary(),
          token: binary(),
          amount: non_neg_integer(),
          locked: non_neg_integer(),
          # deferred: non_neg_integer(),
          # tx_count: non_neg_integer(),
          created_at: integer(),
          updated_at: integer()
        }

  defstruct id: nil,
            token: nil,
            amount: 0,
            locked: 0,
            # deferred: 0,
            # tx_count: 0,
            created_at: nil,
            updated_at: nil

  @impl true
  def to_list(x) do
    [
      x.id,
      x.token,
      x.amount,
      x.locked,
      x.created_at,
      x.updated_at
    ]
  end

  @impl true
  def list_to_tuple(x) do
    map = list_to_map(x)
    {{map.id, map.token}, map}
  end

  @impl true
  def to_tuple(x) do
    {{x.id, x.token}, x}
  end

  @impl true
  def to_map({_id_token, map}), do: map

  @impl true
  def list_to_map([id, token, amount, locked, created_at, updated_at]) do
    %{
      id: id,
      token: token,
      amount: amount,
      locked: locked,
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
