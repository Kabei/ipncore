defmodule ValidatorStore do
  @table "validator"

  use Store.Sqlite,
    base: :validator,
    cache: true,
    mod: Ippan.Validator,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hostname VARCHAR(50) UNIQUE NOT NULL,
      name VARCHAR(30) NOT NULL,
      owner BLOB NOT NULL,
      address BLOB NOT NULL,
      avatar TEXT,
      enabled BOOLEAN DEFAULT TRUE,
      fee DOUBLE NOT NULL,
      fee_type TINYINT NOT NULL,
      created_at UNSIGNED BIGINT NOT NULL,
      updated_at UNSIGNED BIGINT NOT NULL
    );
    """,
    stmt: %{
      "lookup_address" => "SELECT * FROM #{@table} WHERE name = ?1",
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)",
      # INSERT INTO #{@table} (id,hostname,name,owner,avatar,enabled,fee,created_at,updated_at)
      # values($1,$2,$3,$4,$5,$6,$7,$8,$9)
      # ON CONFLICT (id, hostname) DO UPDATE SET
      # name = EXCLUDED.name, owner = EXCLUDED.owner,
      # avatar = EXCLUDED.avatar, fee = EXCLUDED.fee,
      # enabled = EXCLUDED.enabled, created_at = EXCLUDED.created_at,
      # updated_at = EXCLUDED.updated_at WHERE created_at < EXCLUDED.created_at;
      lookup: "SELECT * FROM #{@table} WHERE name = ?1",
      exists: "SELECT 1 FROM #{@table} WHERE name = ?1",
      delete: "DELETE FROM #{@table} WHERE name = ?1 AND owner = ?2",
      owner: "SELECT 1 FROM #{@table} WHERE name = ?1 AND owner = ?2"
    }
end
