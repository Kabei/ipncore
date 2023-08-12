defmodule BigNumber do
  @moduledoc """
  BigNumber is a module that operate number in binary format.
  * BigNumber is a positive integer value into binary
  """
  defmacro to_big_number(number) do
    quote do
      :binary.encode_unsigned(unquote(number))
    end
  end

  defmacro from_big_number(bin) do
    quote do
      :binary.decode_unsigned(unquote(bin))
    end
  end

  def add(bin, number) do
    (from_big_number(bin) + number) |> to_big_number()
  end

  def sub(bin, number) do
    (from_big_number(bin) - number) |> to_big_number()
  end

  def mult(bin, number) do
    (from_big_number(bin) * number) |> to_big_number()
  end

  def div(bin, number) do
    Kernel.div(from_big_number(bin), number) |> to_big_number()
  end

  def gt?(bin1, bin2) do
    byte_size(bin1) > byte_size(bin2) or bin1 > bin2
  end

  def lt?(bin1, bin2) do
    byte_size(bin1) < byte_size(bin2) or bin1 < bin2
  end

  def eq?(bin1, bin2) do
    bin1 == bin2
  end
end
