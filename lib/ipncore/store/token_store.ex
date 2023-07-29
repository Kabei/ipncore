defmodule TokenStore do
  @table "token"

  alias Ippan.Token

  use Store.Sqlite2,
    base: :token,
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
      max_supply BIGINT DEFAULT 0,
      props BLOB,
      created_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL
    ) WITHOUT ROWID"],
    stmt: %{
      "sum_supply" =>
        ~c"UPDATE #{@table} SET supply = supply + ?2 WHERE id = ?1 AND (max_supply = 0 OR max_supply >= supply + ?2)",
      "sum_burned" =>
        ~c"UPDATE #{@table} SET burned = burned + ?2, supply = supply - ?2 WHERE id = ?1",
      "owner_props" => ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2 AND props LIKE ?3",
      "props" => ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND props LIKE ?2",
      insert: ~c"INSERT INTO #{@table} VALUES(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12,?13)",
      lookup: ~c"SELECT * FROM #{@table} WHERE id = ?",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id = ?",
      delete:
        ~c"DELETE FROM #{@table} WHERE id = ?1 AND owner = ?2 AND supply = 0 AND burned = 0",
      owner: ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      total: ~c"SELECT COUNT(1) FROM #{@table}"
    }

  use Store.Cache,
    table: :token,
    mod: Token,
    mode: "partial",
    size: 10_000_000

  def sum_suppy(token, total) do
    call({:step, "sum_supply", [token, total]})
  end

  def sum_burned(token, total) do
    call({:step, "sum_burned", [token, total]})
  end

  def total do
    {_, [total]} = call({:step, :total, []})
    total
  end
end
