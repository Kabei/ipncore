defmodule EnvStore do
  @table "env"

  alias Ippan.Env

  use Store.Sqlite2,
    base: :env,
    table: @table,
    mod: Ippan.Env,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
    name TEXT PRIMARY KEY NOT NULL,
    value BLOB,
    created_at BIGINT NOT NULL
    ) WITHOUT ROWID;
    "],
    stmt: %{
      insert: ~c"REPLACE INTO #{@table} values(?1, ?2, ?3)",
      delete: ~c"DELETE FROM #{@table} WHERE name=?1",
      lookup: ~c"SELECT value FROM #{@table} WHERE name=?1"
    }

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
      {_name, x, _} when is_integer(x) and x > 0 -> x
      _ -> 1_000
    end
  end
end
