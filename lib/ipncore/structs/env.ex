defmodule Ippan.Env do
  @type t :: %__MODULE__{
          name: String.t(),
          value: binary(),
          timestamp: non_neg_integer()
        }

  defstruct [:name, :value, :timestamp]

  def to_list(x) do
    [
      x.name,
      x.value,
      x.timestamp
    ]
  end

  def to_tuple(x) do
    {x.name, x.value, x.timestamp}
  end

  def to_map({name, value, timestamp}) do
    %{name: name, value: value, timestamp: timestamp}
  end

  def to_map([name, value, timestamp]) do
    %{name: name, value: value, timestamp: timestamp}
  end
end
