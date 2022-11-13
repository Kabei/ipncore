defmodule Ipncore.Utils do
  def to_keywords(params) do
    params
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  def to_keywords(params, filter) do
    params
    |> Enum.take(filter)
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
  end
end
