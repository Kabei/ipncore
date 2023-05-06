defmodule EnvStore do
  @table "env"

  use Store.Sqlite,
    base: :env,
    table: @table,
    cache: true,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
    name TEXT PRIMARY KEY NOT NULL,
    value BLOB
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "REPLACE INTO #{@table} values(?1, ?2)",
      delete: "DELETE FROM #{@table} WHERE name=?1",
      lookup: "SELECT value FROM #{@table} WHERE name=?1"
    }

    def get(name, default \\ nil) do
      lookup(name) || default
    end
end
