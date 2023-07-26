defmodule DnsStore do
  @table "dns"

  use Store.Sqlite2,
    base: :dns,
    table: @table,
    mod: Ippan.DNS,
    create: ~c"CREATE TABLE IF NOT EXISTS #{@table}(
      domain TEXT NOT NULL,
      name TEXT NOT NULL,
      type TINYINT NOT NULL,
      data TEXT,
      ttl integer DEFAULT 0,
      hash BLOB NOT NULL,
      PRIMARY KEY(domain, hash)
    );",
    stmt: %{
      "delete_hash" => ~c"DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND hash=?3",
      "delete_type" => ~c"DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND type=?3",
      "delete_name" => ~c"DELETE FROM #{@table} WHERE domain = ?1 AND name=?2",
      delete: ~c"DELETE FROM #{@table} WHERE domain = ?1",
      insert: ~c"INSERT INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6)",
      replace: ~c"REPLACE INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6)",
      lookup: ~c"SELECT * FROM #{@table} WHERE domain=?1 AND hash=?2",
      exists: ~c"SELECT 1 FROM #{@table} WHERE domain=?1 AND hash=?2"
    }

  def replace(params) do
    call({:step, :replace, params})
  end
end
