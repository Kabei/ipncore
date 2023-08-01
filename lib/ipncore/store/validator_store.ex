defmodule ValidatorStore do
  @table "validator"

  use Store.Sqlite2,
    base: :validator,
    cache: true,
    mod: Ippan.Validator,
    table: @table,
    create: "
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hostname VARCHAR(50) UNIQUE NOT NULL,
      name VARCHAR(30) NOT NULL,
      owner BLOB NOT NULL,
      pubkey BLOB NOT NULL,
      net_pubkey BLOB NOT NULL,
      avatar TEXT,
      fee_type TINYINT NOT NULL,
      fee DOUBLE NOT NULL,
      stake BIGINT NOT NULL DEFAULT 0,
      created_at BIGINT NOT NULL,
      updated_at BIGINT NOT NULL
      );",
    stmt: %{
      "owner" => ~c"SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      insert: ~c"INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      lookup: ~c"SELECT * FROM #{@table} WHERE id = ?1",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id = ?1",
      delete: ~c"DELETE FROM #{@table} WHERE id = ?1",
      total: ~c"SELECT COUNT(1) FROM #{@table}"
    }

  use Store.Cache,
    table: :validator,
    mod: Ippan.Validator,
    mode: "partial",
    size: 10_000_000

  def total do
    {_, [total]} = call({:step, :total, []})
    total
  end
end
