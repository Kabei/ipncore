defmodule ValidatorStore do
  @table "validator"

  use Store.Sqlite,
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
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?1",
      owner: "SELECT 1 FROM #{@table} WHERE id = ?1 AND owner = ?2",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?1",
      delete: "DELETE FROM #{@table} WHERE id = ?1",
      total: "SELECT COUNT(id) FROM #{@table}"
    }

    def total do
      {_, [total]} = call({:execute_step, :total, []})
      total
    end
end
