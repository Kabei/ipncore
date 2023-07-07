defmodule DomainStore do
  @table "domain"
  @table_df "domain_df"

  use Store.Sqlite,
    base: :domain,
    table: @table,
    mod: Ippan.Domain,
    create: [
      """
      CREATE TABLE IF NOT EXISTS #{@table}(
      name TEXT PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      email TEXT,
      avatar TEXT,
      records BIGINT DEFAULT 0,
      enabled BOOLEAN DEFAULT TRUE,
      created_at BIGINT NOT NULL,
      renewed_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL
      ) WITHOUT ROWID;
      """,
      """
      CREATE TABLE IF NOT EXISTS #{@table_df}(
      name TEXT PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      email TEXT,
      avatar TEXT,
      records BIGINT DEFAULT 0,
      enabled BOOLEAN DEFAULT TRUE,
      created_at BIGINT NOT NULL,
      renewed_at BIGINT NOT NULL,
      hash BLOB NOT NULL,
      round BIGINT NOT NULL
      ) WITHOUT ROWID;
      """
    ],
    stmt: %{
      insert: "INSERT INTO #{@table} VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
      insert_deferred:
        "INSERT INTO #{@table_df} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10) ON CONFLICT (name)
        DO UPDATE SET owner=?2, email=?3, avatar=?4, records=?5, enabled=?6, created_at=?7, renewed_at=?8, hash=?9, round=?10
        WHERE created_at > EXCLUDED.created_at OR created_at = EXCLUDED.created_at AND hash > EXCLUDED.hash",
      replace: "REPLACE INTO #{@table} VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
      lookup: "SELECT * FROM #{@table} WHERE name = ?",
      exists: "SELECT 1 FROM #{@table} WHERE name = ?",
      owner: "SELECT 1 FROM #{@table} WHERE name = ? AND owner = ?",
      renew:
        "UPDATE #{@table} SET renewed_at = renewed_at + ?3, updated_at = ?4 WHERE name=?1 AND owner=?2",
      delete: "DELETE FROM #{@table} WHERE name = ?",
      move:
        "INSERT OR IGNORE INTO #{@table} (name, owner, email, avatar, records, enabled, created_at, renewed_at, updated_at)
        SELECT name, owner, email, avatar, records, enabled, created_at, renewed_at, created_at FROM #{@table_df} WHERE round=?1",
      delete_deferred: "DELETE FROM #{@table_df} WHERE round=?1"
    }

  def renew(name, account_id, millis, timestamp) do
    call({:execute_changes, :renew, [name, account_id, millis, timestamp]})
  end
end
