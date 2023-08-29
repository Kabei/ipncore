defmodule Ippan.Env do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          name: String.t(),
          value: binary(),
          timestamp: non_neg_integer()
        }

  defstruct [:name, :value, :timestamp]

  @impl true
  def to_list(x) do
    [
      x.name,
      :erlang.term_to_binary(x.value),
      x.timestamp
    ]
  end

  @impl true
  def list_to_tuple([name | _] = x) do
    {name, list_to_map(x)}
  end

  @impl true
  def list_to_map([name, value, timestamp]) do
    %{name: name, value: :erlang.binary_to_term(value), timestamp: timestamp}
  end

  @impl true
  def to_map({_name, map}), do: map

  @impl true
  def to_tuple(x) do
    {x.name, x}
  end
end
