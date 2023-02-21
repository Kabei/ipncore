defmodule Ipncore.Util do
  def empty?(nil), do: true
  def empty?(<<>>), do: true
  def empty?([]), do: true
  def empty?(x) when x == %{}, do: true
  #   def empty?(0), do: true
  #   def empty?(false), do: true
  def empty?(_), do: false

  def to_decimal(number, 0), do: number

  def to_decimal(number, decimals) do
    (number / :math.pow(10, decimals))
    |> Decimal.from_float()

    # |> :erlang.float_to_binary([:compact, decimals: 18])
  end
end
