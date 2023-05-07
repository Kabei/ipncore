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

  def to_list(x) do
    [x.type, x.timestamp, x.from, x.args, x.signature]
  end

  def to_map({type, timestamp, from, args, signature}) do
    %{
      type: type,
      timestamp: timestamp,
      from: from,
      args: args,
      signature: signature
    }
  end

  def to_tuple(x) do
    {x.type, x.timestamp, x.from, x.args, x.signature}
  end
end
