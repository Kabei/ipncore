defmodule DomainStore do
  @table "domain"

  use Store.Sqlite,
    base: :domain,
    table: @table,
    mod: Ippan.Domain,
    create: "CREATE TABLE IF NOT EXISTS #{@table}(
      name TEXT PRIMARY KEY NOT NULL,
      owner BLOB NOT NULL,
      email TEXT,
      avatar TEXT,
      records BIGINT DEFAULT 0,
      enabled BOOLEAN DEFAULT TRUE,
      created_at BIGINT NOT NULL,
      renewed_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL
      ) WITHOUT ROWID",
    stmt: %{
      owner: "SELECT 1 FROM #{@table} WHERE name = ?1 AND owner = ?2",
      insert: "INSERT INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
      lookup: "SELECT * FROM #{@table} WHERE name = ?1",
      exists: "SELECT 1 FROM #{@table} WHERE name = ?1",
      renew:
        "UPDATE #{@table} SET renewed_at = renewed_at + ?3, updated_at = ?4 WHERE name=?1 AND owner=?2",
      delete: "DELETE FROM #{@table} WHERE name = ?1"
    }

  def renew(name, account_id, millis, timestamp) do
    call({:execute_changes, :renew, [name, account_id, millis, timestamp]})
  end
end
