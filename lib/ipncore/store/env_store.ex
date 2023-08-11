defmodule EnvStore do
  alias Ippan.Env
  @table "env"

  use Store.Sqlite2,
    base: :env,
    table: @table,
    mod: Ippan.Env,
    create: SQL.readFile!("lib/sql/env.sql"),
    stmt: SQL.readFileStmt!("lib/sql/env.stmt.sql")

  use Store.Cache,
    table: :env,
    mod: Env,
    mode: "full"

  def put(name, value, timestamp) do
    :ets.insert(:env, {name, value, timestamp})
    cast({:step, :insert, [name, :erlang.term_to_binary(value), timestamp]})
  end

  def token_price do
    case get("TOKEN.PRICE") do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 50000
    end
  end

  def validator_stake do
    case get("VALIDATOR.STAKE") do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 100_000
    end
  end

  def jackpot_reward do
    case get("JACKPOT.REWARD") do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 100
    end
  end

  def network_fee do
    case get("NETWORK.FEE") do
      {_name, x, _} when is_integer(x) and x >= 0 -> x
      _ -> 1
    end
  end
end
