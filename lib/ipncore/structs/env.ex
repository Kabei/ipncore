defmodule Ippan.Env do
  @type t :: %__MODULE__{
          name: String.t(),
          value: binary(),
          ttl: integer()
        }

  defstruct [:name, :value, ttl: -1]

  def to_list(x) do
    [
      x.name,
      x.value,
      x.ttl
    ]
  end

  def to_tuple(x) do
    {x.name, x.value, x.ttl}
  end

  def to_map({name, value, ttl}) do
    %{name: name, value: value, ttl: ttl}
  end

  def to_map([name, value, ttl]) do
    %{name: name, value: value, ttl: ttl}
  end
end
