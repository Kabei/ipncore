defmodule DnsStore do
  @table "dns"

  use Store.Sqlite,
    base: :dns,
    table: @table,
    mod: Ippan.DNS,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      domain TEXT NOT NULL,
      name TEXT NOT NULL,
      type TINYINT NOT NULL,
      data TEXT,
      ttl integer DEFAULT 0,
      hash BLOB NOT NULL
    );
    """,
    stmt: %{
      "delete_hash" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND hash=?3",
      "delete_type" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND type=?3",
      "delete_name" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2",
      delete: "DELETE FROM #{@table} WHERE domain = ?1",
      insert: "INSERT INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6)",
      replace: "REPLACE INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5, ?6)",
      lookup: "SELECT * FROM #{@table} WHERE domain=?1 AND hash=?2",
      exists: "SELECT 1 FROM #{@table} WHERE domain=?1 AND hash=?2"
    }
end
