defmodule EnvStore do
  require Sqlite

  def all(db_ref) do
    data = Sqlite.all("all_env")

    Enum.map(data, fn
      [name, value] ->
        {name, :erlang.binary_to_term(value)}

      [name, nil] ->
        {name, nil}
    end)
    |> Map.new()
  end

  def load(db_ref) do
    data = Sqlite.all("all_env")

    Enum.each(data, fn [name, value] ->
      :persistent_term.put({:env, name}, :erlang.binary_to_term(value))
    end)
  end

  def put(db_ref, name, value) do
    value = transform(name, value)
    :persistent_term.put({:env, name}, value)
    Sqlite.step("insert_env", [name, :erlang.term_to_binary(value)])
  end

  def get(name, default \\ nil) do
    :persistent_term.put({:env, name}, default)
  end

  def delete(db_ref, name) do
    :persistent_term.erase({:env, name})
    Sqlite.step("delete_env", [name])
  end

  def owner, do: :persistent_term.get({:env, "owner"}, nil)

  def service_price do
    :persistent_term.get({:env, "service.price"}, 50_000)
  end

  def token_price do
    :persistent_term.get({:env, "token.price"}, 50_000)
  end

  def validator_price do
    :persistent_term.get({:env, "validator.price"}, 100_000)
  end

  def block_limit do
    :persistent_term.get({:env, "block.limit"}, 1)
  end

  def min_fa do
    :persistent_term.get({:env, "fees.fa"}, 0)
  end

  def min_fb do
    :persistent_term.get({:env, "fees.fb"}, 1)
  end

  def max_failures do
    :persistent_term.get({:env, "validator.max_failures"}, 0)
  end

  defp transform("owner", x) when byte_size(x) <= 255, do: x
  defp transform("owner", _x), do: nil

  defp transform("service.price", x) when is_integer(x) and x > 0, do: x
  defp transform("service.price", _), do: 50_000

  defp transform("token.price", x) when is_integer(x) and x > 0, do: x
  defp transform("token.price", _), do: 100_000

  defp transform("validator.price", x) when is_integer(x) and x > 0, do: x
  defp transform("validator.price", _), do: 100_000

  defp transform("block.limit", x) when x in 1..100, do: x
  defp transform("block.limit", _), do: 10

  defp transform("validator.max_failures", x) when x in 0..1000, do: x
  defp transform("validator.max_failures", _), do: 0

  defp transform(_, x), do: x
end
