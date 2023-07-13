defmodule TokenStore do
  @table "token"
  # table with transaction deferred
  @table_df "token_df"

  use Store.Sqlite,
    base: :token,
    cache: true,
    mod: Ippan.Token,
    table: @table,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id VARCHAR(20) PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      name TEXT NOT NULL,
      avatar TEXT,
      decimal TINYINT DEFAULT 0,
      symbol VARCHAR(5) NOT NULL,
      enabled BOOLEAN,
      supply BIGINT DEFAULT 0,
      burned BIGINT DEFAULT 0,
      props BLOB,
      created_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL
    ) WITHOUT ROWID;
    ", "
    CREATE TABLE IF NOT EXISTS #{@table_df}(
      id VARCHAR(20) PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      name TEXT NOT NULL,
      avatar TEXT,
      decimal TINYINT DEFAULT 0,
      symbol VARCHAR(5) NOT NULL,
      enabled BOOLEAN,
      supply BIGINT DEFAULT 0,
      burned BIGINT DEFAULT 0,
      props BLOB,
      created_at BIGINT NOT NULL,
      hash BLOB NOT NULL,
      round BIGINT NOT NULL
    ) WITHOUT ROWID;
    "],
    # WHERE timestamp > ?4 OR timestamp == ?4 AND hash > ?3
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      replace: "REPLACE INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      delete: "DELETE FROM #{@table} WHERE id = ? AND owner = ? AND supply = 0 AND burned = 0",
      sum_supply: "UPDATE #{@table} SET supply = supply + ?2 WHERE id = ?1",
      sum_burned: "UPDATE #{@table} SET burned = burned + ?2 WHERE id = ?1",
      owner: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      owner_props: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2 AND props LIKE ?3",
      props: "SELECT 1 FROM #{@table} WHERE id = ?1 AND props LIKE ?2",
      move:
        "INSERT OR IGNORE INTO #{@table} (id, owner, name, avatar, decimal, symbol, enabled, supply, burned, props, created_at, updated_at)
        SELECT id, owner, name, avatar, decimal, symbol, enabled, supply, burned, props, created_at, created_at FROM #{@table_df} WHERE round=?1",
      delete_deferred: "DELETE FROM #{@table_df} WHERE round=?1"
    }
end
