defmodule Ipncore.Util do
  def empty?(nil), do: true
  def empty?(<<>>), do: true
  def empty?([]), do: true
  def empty?(%{}), do: true
  #   def empty?(0), do: true
  #   def empty?(false), do: true
  def empty?(_), do: false
end
