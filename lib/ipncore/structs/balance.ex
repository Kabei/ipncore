defmodule Ippan.Balance do
  @type t :: %__MODULE__{
          id: binary(),
          token: binary(),
          amount: non_neg_integer(),
          locked: non_neg_integer(),
          # deferred: non_neg_integer(),
          tx_count: non_neg_integer(),
          created_at: integer(),
          updated_at: integer()
        }

  use Ippan.Struct

  defstruct id: nil,
            token: nil,
            amount: 0,
            locked: 0,
            # deferred: 0,
            tx_count: 0,
            created_at: nil,
            updated_at: nil

  def to_list(x) do
    [
      x.id,
      x.token,
      x.amount,
      x.locked,
      # x.deferred,
      x.tx_count,
      x.created_at,
      x.updated_at
    ]
  end

  # def to_list_def(id, type, from, token, to, amount, timestamp, hash, round) do
  #   [id, type, from, token, to, amount, timestamp, hash, round]
  # end

  def to_tuple(x) do
    {
      x.id,
      x.token,
      x.amount,
      x.locked,
      # x.deferred,
      x.tx_count,
      x.created_at,
      x.updated_at
    }
  end

  def to_map({id, token, amount, locked, tx_count, created_at, updated_at}) do
    %{
      id: id,
      token: token,
      amount: amount,
      locked: locked,
      # deferred: deferred,
      tx_count: tx_count,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def to_map([id, token, amount, locked, tx_count, created_at, updated_at]) do
    %{
      id: id,
      token: token,
      amount: amount,
      locked: locked,
      # deferred: deferred,
      tx_count: tx_count,
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
