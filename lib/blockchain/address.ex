defmodule Ipncore.Address do
  def from_text([]), do: []

  def from_text([x | rest]) do
    [from_text(x)] ++ from_text(rest)
  end

  def from_text(x) when byte_size(x) == 20, do: x

  def from_text("1x" <> rest) do
    rest
    |> Base58.decode()
    |> ByteUtils.zeros_pad_leading(160)
  end

  def from_text(""), do: nil
  def from_text(x), do: throw("Error convert address text to bin #{x}")

  def to_text([]), do: []

  def to_text([x | rest]) do
    [to_text(x)] ++ to_text(rest)
  end

  def to_text(x) when byte_size(x) == 20 do
    "1x" <> Base58.encode(x)
  end

  def to_text("1x" <> _rest = x), do: x
  def to_text(nil), do: ""
  def to_text(_), do: throw("Error convert address bin to text")

  def hash(pubkey) do
    :crypto.hash(:ripemd160, pubkey)
  end
end
