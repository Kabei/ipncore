defmodule MapUtil do
  alias Ippan.Address

  ## util functions
  def to_keywords(params) do
    Enum.map(params, fn {k, v} -> {k, v} end)
  end

  def to_keywords(map, filter) do
    map
    |> Map.take(filter)
    |> Enum.map(fn {k, v} -> {k, v} end)
  end

  def to_atom_keywords(params) do
    Enum.map(params, fn {k, v} -> {String.to_atom(k), v} end)
  end

  def to_existing_atom_keywords(params) do
    Enum.map(params, fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  def to_atoms(map) do
    for {k, v} <- map, into: %{}, do: {String.to_atom(k), v}
  end

  def to_atoms(map, filter) do
    for {k, v} <- Map.take(map, filter), into: %{}, do: {String.to_atom(k), v}
  end

  def to_existing_atoms(map) do
    for {k, v} <- map, into: %{}, do: {String.to_existing_atom(k), v}
  end

  def to_existing_atoms(map, filter) do
    for {k, v} <- Map.take(map, filter), into: %{}, do: {String.to_existing_atom(k), v}
  end

  def drop_nils(map) do
    Map.filter(map, fn
      {_, nil} -> false
      _ -> true
    end)
  end

  ## Validation functions
  def validate_not_empty(nil), do: raise(ArgumentError, "Error value is empty")

  def validate_not_empty(x) when map_size(x) == 0,
    do: raise(ArgumentError, "Error value is empty")

  def validate_not_empty(map) when is_map(map), do: map
  def validate_not_empty(_), do: raise(ArgumentError, "Error value is empty")

  def validate_email(map, key) do
    email = Map.get(map, key)

    if not is_nil(email) and not Match.email?(email),
      do: raise(ArgumentError, "Invalid #{key} is not email")

    map
  end

  def validate_url(map, key) do
    url = Map.get(map, key)

    if not is_nil(url) and not Match.url?(url),
      do: raise(ArgumentError, "Invalid #{key} is not a URL")

    map
  end

  def validate_text(map, key) do
    x = Map.get(map, key)

    if not is_nil(x) and not String.valid?(x),
      do: raise(ArgumentError, "Invalid #{key} is not a text valid")

    map
  end

  def validate_map(map, key) do
    value = Map.get(map, key)

    if not is_nil(value) and not is_map(value),
      do: raise(ArgumentError, "Invalid #{key} is not a map")

    map
  end

  def validate_hostname(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Match.hostname?(val),
      do: raise(ArgumentError, "Invalid #{key} is not a hostname")

    map
  end

  def validate_hostname_or_ip(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Match.hostname?(val) and not Match.ipv4?(val),
      do: raise(ArgumentError, "Invalid #{key} is not a hostname")

    map
  end

  def validate_account(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Match.account?(val),
      do: raise(ArgumentError, "Invalid #{key} is not account format")

    map
  end

  def validate_address(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not Match.wallet_address?(val),
      do: raise(ArgumentError, "Invalid #{key} is not payment address")

    map
  end

  def validate_format(map, key, regex) do
    val = Map.get(map, key)

    if not is_nil(val) and not Regex.match?(regex, val),
      do: raise(ArgumentError, "Invalid key: #{key}")

    map
  end

  def validate_boolean(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not is_boolean(val),
      do: raise(ArgumentError, "Invalid #{key} is not boolean")

    map
  end

  def validate_integer(map, key) do
    val = Map.get(map, key)

    if not is_nil(val) and not is_integer(val),
      do: raise(ArgumentError, "Invalid #{key} is not integer")

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
        raise ArgumentError, "Invalid key: #{key}"

      false ->
        map
    end
  end

  def validate_range(map, key, range) do
    val = Map.get(map, key)
    if not is_nil(val) and val not in range, do: raise(ArgumentError, "Invalid range: #{key}")
    map
  end

  def validate_any(map, key, list) when is_list(list) do
    val = Map.get(map, key)

    if not is_nil(val) and not Enum.any?(val, fn x -> x in list end),
      do: raise(ArgumentError, "Invalid key: #{key}")

    map
  end

  def only(map, keys) when is_list(keys) do
    result = Enum.any?(Map.keys(map), fn x -> x not in keys end)

    if result, do: raise(ArgumentError, "Only accepted: #{inspect(keys)}")
    map
  end

  def require(map, keys) do
    result = Enum.all?(Map.keys(map), fn x -> x in keys end)

    if not result, do: raise(ArgumentError, "Error check require values")
    map
  end

  def validate_bytes_range(map, key, range) do
    val = Map.get(map, key)

    if not is_nil(val) and byte_size(val) not in range,
      do: raise(ArgumentError, "Invalid #{key} length exceeded")

    map
  end

  def validate_bytes(map, key, size) do
    case Map.get(map, key) do
      val when byte_size(val) == size ->
        raise ArgumentError, "Invalid #{key} length exceeded"

      _ ->
        map
    end
  end

  def validate_length_range(map, key, _x.._y = range) do
    val = Map.get(map, key)

    if not is_nil(val) and String.length(val) not in range,
      do: raise(ArgumentError, "#{key} has invalid length")

    map
  end

  def validate_length(map, key, size) do
    val = Map.get(map, key)

    if not is_nil(val) and String.length(val) > size,
      do: raise(ArgumentError, "#{key} has invalid length")

    map
  end

  def transform(map, key, fun) do
    val = Map.get(map, key)

    if not is_nil(val) do
      Map.put(map, key, fun.(val))
    else
      map
    end
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
