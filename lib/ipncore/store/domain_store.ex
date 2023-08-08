defmodule DomainStore do
  @table "domain"

  @every div(:timer.hours(24), Application.compile_env(:ipncore, :block_interval))

  use Store.Sqlite2,
    base: :domain,
    table: @table,
    mod: Ippan.Domain,
    create: [
      "CREATE TABLE IF NOT EXISTS #{@table}(
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
      "CREATE UNIQUE INDEX idx_domain_renew ON #{@table}(renewed_at);"
    ],
    stmt: %{
      "delete_expiry" => ~c"DELETE FROM #{@table} WHERE renewed_at < ?",
      owner: ~c"SELECT 1 FROM #{@table} WHERE name = ?1 AND owner = ?2",
      insert: ~c"INSERT INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9)",
      lookup: ~c"SELECT * FROM #{@table} WHERE name = ?1",
      exists: ~c"SELECT 1 FROM #{@table} WHERE name = ?1",
      renew:
        ~c"UPDATE #{@table} SET renewed_at = renewed_at + ?3, updated_at = ?4 WHERE name=?1 AND owner=?2",
      delete: ~c"DELETE FROM #{@table} WHERE name = ?1 AND owner =?2"
    }

  def renew(name, account_id, millis, timestamp) do
    call({:changes, :renew, [name, account_id, millis, timestamp]})
  end

  def delete_expiry(round, timestamp) do
    if round != 0 and rem(round, @every) == 0 do
      call({:step, "delete_expiry", [timestamp]})
      sync()
    end
  end
end
