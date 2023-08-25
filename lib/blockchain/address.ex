defmodule Ippan.Address do
  def from_text(nil), do: nil
  def from_text([]), do: []

  def from_text([x | rest]) do
    [from_text(x) | from_text(rest)]
  end

  def from_text(x) when byte_size(x) == 20, do: x

  def from_text("0x" <> rest) do
    rest
    |> Base58.decode()
    |> ByteUtils.zeros_pad_leading(160)
  end

  def from_text("1x" <> rest) do
    rest
    |> Base58.decode()
    |> ByteUtils.zeros_pad_leading(160)
  end

  def from_text(""), do: nil
  def from_text(x) when byte_size(x) == 20, do: x
  def from_text(x), do: x

  def from_text!(x) do
    result = from_text(x)

    cond do
      is_nil(result) or x == [] or x == "" or result == x ->
        throw("Error convert address text to bin #{x}")

      true ->
        result
    end
  end

  def to_text([]), do: []

  def to_text([x | rest]) do
    [to_text(x) | to_text(rest)]
  end

  def to_text(x) when byte_size(x) == 20 do
    "1x" <> Base58.encode(x)
  end

  def to_text("1x" <> _rest = x), do: x
  def to_text(nil), do: nil
  def to_text(""), do: ""
  def to_text(x), do: x

  def to_text!(x) do
    result = to_text(x)

    cond do
      is_nil(result) or x == [] or x == "" or result == x ->
        throw("Error convert address bin to text #{x}")

      true ->
        result
    end
  end

  @spec hash(type :: non_neg_integer(), pubkey :: binary) :: binary
  def hash(type, pubkey) do
    <<address::bytes-size(20), _::binary>> = Blake3.hash(pubkey)
    IO.iodata_to_binary(["#{type}", "x", Base58.encode(address)])
  end

  @spec get_type(address :: binary()) :: <<_::8>>
  def get_type(address) do
    <<first::bytes-size(1), _rest::binary>> = address
    first
  end
end
