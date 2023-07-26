defmodule EnvStore do
  @table "env"

  alias Ippan.Env

  use Store.Sqlite2,
    base: :env,
    table: @table,
    mod: Ippan.Env,
    create: [~c"
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
    mode: "full",
    size: 10_000_000
end
