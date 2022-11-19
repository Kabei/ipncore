defmodule Ipncore.Utils do
  def to_keywords(params) do
    params
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  def to_keywords(params, filter) do
    params
    |> Enum.take(filter)
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
  end

  def validate_email(map, key) do
    email = Map.get(map, key)

    if !is_nil(email) and not Regex.match(Const.Regex.email(), email),
      do: throw("Invalid #{key}")

    map
  end

  def validate_hostname(map, key) do
    hostname = Map.get(map, key)
    if not Regex.match(Const.Regex.hostname(), hostname), do: throw("Invalid #{key}")

    map
  end

  def validate_boolean(map, key, :boolean) do
    val = Map.get(map, key)
    if not is_boolean(val), do: {:error, "Invalid #{key}"}
    map
  end

  def validate_integer(map, key) do
    val = Map.get(map, key)
    if not is_integer(val), do: {:error, "Invalid #{key}"}
    map
  end

  def require(map, keys) do
    result =
      Map.keys(map)
      |> Enum.all?(fn x -> x in keys end)

    if not result, do: throw("Error check require values")
    map
  end

  def validate_address(map, key) do
    val = Map.get(map, key)
    <<"1x", _::binary>> = val
    map
  end

  def validate_length(map, key, size) do
    val = Map.get(map, key)
    if String.length(val) > size, do: throw("Invalid max length #{key}")

    map
  end

  # def do_cast!(map, key, fun) do
  #   case Map.get(map, key) do
  #     nil ->
  #       map

  #     val ->
  #       case fun.(val) do
  #         :ok ->
  #           map

  #         :delete ->
  #           Map.delete(map, key)

  #         {:reply, k, v} ->
  #           Map.put(map, k, v)

  #         {:error, message} ->
  #           throw(message)
  #       end
  #   end
  # end
end
