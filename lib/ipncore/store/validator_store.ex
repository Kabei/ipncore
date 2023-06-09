defmodule ValidatorStore do
  @table "validator"
  @table_df "validator_df"

  use Store.Sqlite,
    base: :validator,
    cache: true,
    mod: Ippan.Validator,
    table: @table,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hostname VARCHAR(50) UNIQUE NOT NULL,
      name VARCHAR(30) NOT NULL,
      owner BLOB NOT NULL,
      pubkey BLOB NOT NULL,
      avatar TEXT,
      fee_type TINYINT NOT NULL,
      fee DOUBLE NOT NULL,
      created_at UNSIGNED BIGINT NOT NULL,
      updated_at UNSIGNED BIGINT NOT NULL
    );", "CREATE TABLE IF NOT EXISTS #{@table_df}(
      id BIGINT PRIMARY KEY NOT NULL,
      hostname VARCHAR(50) UNIQUE NOT NULL,
      name VARCHAR(30) NOT NULL,
      owner BLOB NOT NULL,
      pubkey BLOB NOT NULL,
      avatar TEXT,
      fee_type TINYINT NOT NULL,
      fee DOUBLE NOT NULL,
      created_at UNSIGNED BIGINT NOT NULL,
      hash BLOB NOT NULL
    );
    "],
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)",
      insert_deferred:
        "INSERT INTO #{@table_df} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10) ON CONFLICT (id)
        DO UPDATE SET hostname=?2, name=?3, owner=?4, pubkey=?5, avatar=?6, fee_type=?7, fee=?8, created_at=?9, hash=?10
        WHERE created_at > EXCLUDED.created_at OR created_at = EXCLUDED.created_at AND hash > EXCLUDED.hash",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
      # INSERT INTO #{@table} (id,hostname,name,owner,avatar,fee,created_at,updated_at)
      # values($1,$2,$3,$4,$5,$6,$7,$8,$9)
      # ON CONFLICT (id, hostname) DO UPDATE SET
      # name = EXCLUDED.name, owner = EXCLUDED.owner,
      # avatar = EXCLUDED.avatar, fee = EXCLUDED.fee,
      # created_at = EXCLUDED.created_at,
      # updated_at = EXCLUDED.updated_at WHERE created_at < EXCLUDED.created_at;
      lookup: "SELECT * FROM #{@table} WHERE id = ?1",
      owner: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?1",
      delete: "DELETE FROM #{@table} WHERE id = ?1",
      move:
        "INSERT OR IGNORE INTO #{@table} (id, hostname, name, owner, pubkey, avatar, fee_type, fee, created_at, updated_at)
        SELECT id, hostname, name, owner, pubkey, avatar, fee_type, fee, created_at, created_at FROM #{@table_df} WHERE round=?1",
      delete_deferred: "DELETE FROM #{@table_df} WHERE round=?1"
    }
end
