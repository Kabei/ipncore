defmodule DNSStore do
  @table "dns"

  use Store.Sqlite,
    base: :domain,
    table: @table,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      domain TEXT NOT NULL,
      name TEXT NOT NULL,
      type TINYINT NOT NULL,
      data TEXT,
      hash BLOB NOT NULL
    );
    """,
    stmt: %{
      "delete_hash" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND hash=?3",
      "delete_type" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2 AND type=?3",
      "delete_name" => "DELETE FROM #{@table} WHERE domain = ?1 AND name=?2",
      "delete_domain" => "DELETE FROM #{@table} WHERE domain = ?1",
      insert: "INSERT INTO #{@table} VALUES(?1, ?2, ?3, ?4, ?5)"
    }
end
