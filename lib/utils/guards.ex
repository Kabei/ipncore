defmodule Guards do
  defguard is_positive(value) when is_integer(value) and value >= 0

  defguard is_binary_size(value, max_size) when is_binary(value) and byte_size(value) == max_size

  defguard is_binary_max_length(value, max_size)
           when is_binary(value) and byte_size(value) <= max_size

  defguard is_float_positive(value) when is_float(value) and value >= 0

  defguard is_fee_type(value) when value in 0..2

  defguard is_wallet_address(value) when byte_size(value) == 20

  defguard between_size(x, n1, n2) when byte_size(x) >= n1 and byte_size(x) <= n2

  defguard check_port(x) when x in 1000..65535
end
