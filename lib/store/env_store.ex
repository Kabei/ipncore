defmodule EnvStore do
  require Sqlite

  def all(db_ref) do
    data = Sqlite.all("all_env")

    Enum.map(data, fn [name, value] ->
      {name, :erlang.binary_to_term(value)}
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

  def owner, do: :persistent_term.get({:env, "OWNER"}, nil)

  def token_price do
    :persistent_term.get({:env, "TOKEN.PRICE"}, 50_000)
  end

  def validator_price do
    :persistent_term.get({:env, "VALIDATOR.PRICE"}, 100_000)
  end

  def fees do
    :persistent_term.get({:env, "FEES"}, 1)
  end

  def burn do
    :persistent_term.get({:env, "BURN"}, 0.3)
  end

  def round_blocks do
    :persistent_term.get({:env, "ROUND.BLOCKS"}, 10)
  end

  defp transform("TOKEN.PRICE", x), do: if(is_integer(x) and x > 0, do: x, else: 50_000)

  defp transform("VALIDATOR.PRICE", x), do: if(is_integer(x) and x > 0, do: x, else: 100_000)

  defp transform("FEES", x), do: if(is_integer(x) and x > 0, do: x, else: 1)

  defp transform("ROUND.BLOCKS", x), do: if(x in 1..100, do: x, else: 10)

  defp transform("BURN", x), do: if(is_float(x) and x >= 0 and x <= 1, do: x, else: 0.3)

  defp transform(_, x), do: x
end
