defmodule MapUtil do
  alias Ipncore.Address

  ## util functions
  def to_keywords(params) do
    Enum.map(params, fn {k, v} -> {k, v} end)
  end

  def to_atom_keywords(params) do
    Enum.map(params, fn {k, v} -> {String.to_atom(k), v} end)
  end

  def to_keywords(map, filter) do
    map
    |> Map.take(filter)
    |> Enum.map(fn {k, v} -> {k, v} end)
  end

  def to_atoms(map) do
    for {k, v} <- map, into: %{}, do: {String.to_atom(k), v}
  end

  def to_atoms(map, filter) do
    for {k, v} <- Map.take(map, filter), into: %{}, do: {String.to_atom(k), v}
  end

  def drop_nils(map) do
    for {k, v} when v <- map != nil, into: %{}, do: {k, v}
  end

  ## Validation functions
  def validate_not_empty(nil), do: throw("Error value is empty")
  def validate_not_empty(x) when x == %{}, do: throw("Error value is empty")
  def validate_not_empty(map) when is_map(map), do: map
  def validate_not_empty(_), do: throw("Error value is empty")

  def validate_email(map, key) do
    email = Map.get(map, key)

    if not is_nil(email) and not Regex.match?(Const.Regex.email(), email),
      do: throw("Invalid #{key}")

    map
  end

  def validate_hostname(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Regex.match?(Const.Regex.hostname(), val),
      do: throw("Invalid #{key}")

    map
  end

  def validate_address(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Regex.match?(Const.Regex.address(), val),
      do: throw("Invalid address #{key}")

    map
  end

  def validate_format(map, key, regex) do
    val = Map.get(map, key)
    if not is_nil(val) and not Regex.match?(regex, val), do: throw("Invalid #{key}")

    map
  end

  def validate_boolean(map, key) do
    val = Map.get(map, key)
    if not is_nil(val) and not is_boolean(val), do: throw("Invalid #{key}")
    map
  end

  def validate_integer(map, key) do
    val = Map.get(map, key)
    if not is_nil(val) and not is_integer(val), do: throw("Invalid #{key}")
    map
  end

  def validate_value(map, key, op, value) do
    val = Map.get(map, key)

    (not is_nil(val) and
       case op do
         :gt -> value > val
         :eq -> value == val
         :lt -> value < val
         :gte -> value >= val
         :lte -> value <= val
       end)
    |> case do
      true ->
        throw("Invalid #{key}")

      false ->
        map
    end
  end

  def validate_range(map, key, range) do
    val = Map.get(map, key)
    if not is_nil(val) and val not in range, do: throw("Invalid range #{key}")
    map
  end

  def validate_any(map, key, list) when is_list(list) do
    val = Map.get(map, key)

    if not is_nil(val) and not Enum.any?(val, fn x -> x in list end),
      do: throw("Invalid range #{key}")

    map
  end

  def require_only(map, keys) do
    result = Enum.all?(Map.keys(map), fn x -> x in keys end)

    if not result, do: throw("Error check require values")
    map
  end

  def validate_bytes(map, key, _x.._y = range) do
    val = Map.get(map, key)
    if not is_nil(val) and byte_size(val) not in range, do: throw("Invalid max length #{key}")

    map
  end

  def validate_bytes(map, key, size, _) do
    val = Map.get(map, key)
    if not is_nil(val) and byte_size(val) > size, do: throw("Invalid max length #{key}")

    map
  end

  def validate_length(map, key, _x.._y = range) do
    val = Map.get(map, key)
    if not is_nil(val) and String.length(val) not in range, do: throw("Invalid max length #{key}")

    map
  end

  def validate_length(map, key, size) do
    val = Map.get(map, key)
    if not is_nil(val) and String.length(val) > size, do: throw("Invalid max length #{key}")

    map
  end

  ## Encode/Decode functions
  def decode_address(map, key) do
    val = Map.get(map, key)

    case val do
      nil ->
        map

      val ->
        Map.put(map, key, Address.from_text!(val))
    end
  end

  def encode_address(map, key) do
    val = Map.get(map, key)

    case val do
      nil ->
        map

      val ->
        Map.put(map, key, Address.to_text(val))
    end
  end
end
