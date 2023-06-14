defmodule EnvStore do
  @table "env"

  use Store.Sqlite,
    base: :env,
    table: @table,
    cache: false,
    mod: Ippan.Env,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
    name TEXT PRIMARY KEY NOT NULL,
    value BLOB,
    created_at BIGINT NOT NULL
    ) WITHOUT ROWID;
    "],
    stmt: %{
      insert: "REPLACE INTO #{@table} values(?1, ?2, ?3)",
      delete: "DELETE FROM #{@table} WHERE name=?1",
      lookup: "SELECT value FROM #{@table} WHERE name=?1"
    }

  def get(name, default \\ nil) do
    case lookup(name) do
      nil ->
        default

      val ->
        :erlang.binary_to_term(val)
    end
  end
end
