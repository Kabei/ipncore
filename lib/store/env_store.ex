defmodule EnvStore do
  require SqliteStore

  def load(conn, stmts) do
    {:ok, data} = SqliteStore.all(conn, stmts, "all_env")

    for [name, value] <- data do
      :persistent_term.put(name, :erlang.binary_to_term(value))
    end
  end

  def put(conn, stmts, name, value, timestamp) do
    :persistent_term.put({:env, name}, value)
    SqliteStore.step(conn, stmts, "insert_env", [name, :erlang.term_to_binary(value), timestamp])
  end

  def delete(conn, stmts, name) do
    :persistent_term.erase({:env, name})
    SqliteStore.step(conn, stmts, "delete_env", [name])
  end

  def token_price do
    case :persistent_term.get({:env, "TOKEN.PRICE"}, 50_000) do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 50_000
    end
  end

  def validator_stake do
    case :persistent_term.get({:env, "VALIDATOR.STAKE"}, 100_000) do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 100_000
    end
  end

  def jackpot_reward do
    case :persistent_term.get({:env, "JACKPOT.REWARD"}, 100) do
      {_name, x, _} when is_integer(x) and x >= 0 and x <= 10_000 -> x
      _ -> 100
    end
  end

  def network_fee do
    case :persistent_term.get({:env, "NETWORK.FEE"}, 1) do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 1
    end
  end

  def blocks_per_round do
    case :persistent_term.get({:env, "ROUND.BLOCKS"}, 10) do
      {_name, x, _} when x in 0..100 -> x
      _ -> 10
    end
  end
end
