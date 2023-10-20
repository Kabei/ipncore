defmodule Tools do
  def fees!(%{validator: %{validator: %{fee_type: fee_type, fee: fee}}, size: size}, amount) do
    calc_fees!(fee_type, fee, amount, size)
  end

  #  Fee types:
  #  0 -> by size
  #  1 -> by percent
  #  2 -> fixed price
  # by size
  defp calc_fees!(0, fee_amount, _tx_amount, size),
    do: trunc(fee_amount) * size

  # by percent
  defp calc_fees!(1, fee_amount, tx_amount, _size),
    do: :math.ceil(tx_amount * fee_amount) |> trunc()

  # fixed price
  defp calc_fees!(2, fee_amount, _tx_amount, _size), do: trunc(fee_amount)

  defp calc_fees!(_, _, _, _), do: raise(IppanError, "Fee calculation error")
end
