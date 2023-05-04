defmodule TokenStore do
  @table "token"

  use Store.Sqlite,
    base: :token,
    cache: true,
    mod: Ippan.Token,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id VARCHAR(20) PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      name TEXT NOT NULL,
      avatar TEXT NOT NULL,
      decimal TINYINT DEFAULT 0,
      symbol VARCHAR(5) NOT NULL,
      enabled BOOLEAN,
      supply UNSIGNED BIGINT DEFAULT 0,
      burned UNSIGNED BIGINT DEFAULT 0,
      props BLOB,
      created_at UNSIGNED BIGINT NOT NULL,
      updated_at UNSIGNED BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      delete: "DELETE FROM #{@table} WHERE id = ? AND owner = ? AND supply = 0 AND burned = 0",
      sum_supply: "UPDATE #{@table} SET supply = supply + ?2 WHERE id = ?1",
      sum_burned: "UPDATE #{@table} SET burned = burned + ?2 WHERE id = ?1",
      owner: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      owner_props: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2 AND props LIKE ?3",
      props: "SELECT 1 FROM #{@table} WHERE id = ?1 AND props LIKE ?2"
    }
end
