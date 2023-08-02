defmodule Ippan.Env do
  @type t :: %__MODULE__{
          name: String.t(),
          value: binary(),
          timestamp: non_neg_integer()
        }

  defstruct [:name, :value, :timestamp]

  def encode_list(x) do
    [
      x.name,
      :erlang.term_to_binary(x.value),
      x.timestamp
    ]
  end

  def decode_ets(x) do
    {x.name, :erlang.binary_to_term(x.value), x.timestamp}
  end

  def decode_map([name, value, timestamp]) do
    %{name: name, value: :erlang.binary_to_term(value), timestamp: timestamp}
  end
end
