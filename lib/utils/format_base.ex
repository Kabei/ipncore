defmodule FormatBase do
  defmacro __using__(alphabet) do
    alphabet = Enum.with_index(to_charlist(alphabet))
    len = length(alphabet)

    funcs =
      for {x, index} <- alphabet do
        quote do
          defp char_to_index(unquote(x)), do: unquote(index)
          defp index_to_char(unquote(index)), do: unquote(x)
        end
      end

    quote do
      @len unquote(len)

      unquote(funcs)

      @spec encode(binary()) :: String.t()
      def encode(x), do: encode(:binary.decode_unsigned(x), <<>>)

      defp encode(0, <<>>), do: <<index_to_char(0)>>
      defp encode(0, acc), do: acc

      defp encode(x, acc) do
        encode(div(x, @len), <<index_to_char(rem(x, @len)), acc::binary>>)
      end

      @spec decode(String.t()) :: binary()
      def decode(enc), do: :binary.encode_unsigned(decode(enc, 0))

      defp decode(<<>>, acc), do: acc

      defp decode(<<c, rest::binary>>, acc) do
        decode(rest, acc * @len + char_to_index(c))
      end
    end
  end
end
