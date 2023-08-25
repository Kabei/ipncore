defmodule BigNumber do
  @moduledoc """
  BigNumber is a module that operate number in binary format.
  * BigNumber is a positive integer value into binary
  """
  defmacro to_bin(number) do
    quote do
      :binary.encode_unsigned(unquote(number))
    end
  end

  defmacro to_int(bin) do
    quote do
      :binary.decode_unsigned(unquote(bin))
    end
  end

  def add(bin, number) do
    :erlang.+(to_int(bin), number) |> to_bin()
  end

  def sub(bin, number) do
    :erlang.-(to_int(bin), number) |> to_bin()
  end

  def mult(bin, number) do
    :erlang.*(to_int(bin), number) |> to_bin()
  end

  def div(bin, number) do
    :erlang.div(to_int(bin), number) |> to_bin()
  end

  def gt?(bin1, bin2) do
    :erlang.byte_size(bin1) > :erlang.byte_size(bin2) or bin1 > bin2
  end

  def lt?(bin1, bin2) do
    :erlang.byte_size(bin1) < :erlang.byte_size(bin2) or bin1 < bin2
  end

  defmacro eq?(bin1, bin2) do
    quote do
      :erlang.==(unquote(bin1), unquote(bin2))
    end
  end
end
