defmodule Ippan.Utils do
  def empty?(nil), do: true
  def empty?(<<>>), do: true
  def empty?([]), do: true
  def empty?(x) when x == %{}, do: true
  #   def empty?(0), do: true
  #   def empty?(false), do: true
  def empty?(_), do: false

  def to_atom(nil), do: nil
  def to_atom(text), do: String.to_atom(text)

  # def to_decimal(number, 0), do: number

  # def to_decimal(number, decimals) do
  #   (number / :math.pow(10, decimals))
  #   |> Decimal.from_float()

  #   # |> :erlang.float_to_binary([:compact, decimals: 18])
  # end

  @spec rows_to_columns(map() | Keyword.t()) :: {list(), list()}
  def rows_to_columns(map_or_kw) do
    result =
      cond do
        is_map(map_or_kw) ->
          Map.to_list(map_or_kw)

        is_list(map_or_kw) ->
          Keyword.to_list(map_or_kw)
      end
      |> Enum.reduce(%{keys: [], values: []}, fn {key, value}, acc ->
        %{keys: [key | acc.keys], values: [value | acc.values]}
      end)

    keys = Enum.reverse(result.keys)
    values = Enum.reverse(result.values)

    {keys, values}
  end

  def estimate_size(term, depth \\ 0) do
    case term do
      bin when is_binary(bin) ->
        byte_size(bin)

      num when is_number(num) ->
        8

      enum when is_map(enum) or is_list(enum) ->
        Enum.reduce(enum, 0, fn term, size -> size + estimate_size(term, depth + 1) end)

      tuple when is_tuple(tuple) ->
        estimate_size(Tuple.to_list(tuple), depth)

      atom when is_atom(atom) ->
        8
    end
  end

  #  Fee types:
  #  0 -> by size
  #  1 -> by percent
  #  2 -> fixed price
  # by size
  def calc_fees!(0, fee_amount, _tx_amount, size),
    do: trunc(fee_amount) * size

  # by percent
  def calc_fees!(1, fee_amount, tx_amount, _size),
    do: :math.ceil(tx_amount * fee_amount) |> trunc()

  # fixed price
  def calc_fees!(2, fee_amount, _tx_amount, _size), do: trunc(fee_amount)

  def calc_fees!(_, _, _, _), do: raise(IppanError, "Fee calculation error")

  def get_name_from_node(node_name) do
    node_name |> to_string() |> String.split("@") |> hd
  end

  def my_ip do
    node() |> to_string() |> String.split("@") |> List.last()
  end

  def get_random_node_verifier do
    Node.list() |> Enum.random() |> to_string() |> String.split("@") |> hd
  end
end
