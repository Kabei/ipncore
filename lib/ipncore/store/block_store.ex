defmodule BlockStore do
  @table "block"

  use Store.Sqlite,
    base: :block,
    table: @table,
    mod: Ippan.Block,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT UNIQUE,
      height BIGINT,
      validator BIGINT NOT NULL,
      hash BLOB,
      prev BLOB,
      signature BLOB,
      hashfile BLOB NOT NULL,
      round BIGINT,
      timestamp BIGINT NOT NULL,
      ev_count BIGINT DEFAULT 0,
      size BIGINT,
      vsn SMALLINT NOT NULL,
      UNIQUE(height, validator) ON CONFLICT REPLACE
    );
    """,
    stmt: %{
      insert:
        "INSERT INTO #{@table}(height, validator, hashfile, round, timestamp, ev_count, size, vsn) values(?1,?2,?3,?4,?5,?6,?7,?8)",
      # insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10)",
      replace: "REPLACE INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11,?12)",
      lookup: "SELECT * FROM #{@table} WHERE height = ?",
      lookup_hash: "SELECT * FROM #{@table} WHERE hash = ? LIMIT 1",
      exists: "SELECT 1 FROM #{@table} WHERE height = ?",
      delete: "DELETE FROM #{@table} WHERE height = ?",
      count: "SELECT count(*) FROM #{@table} WHERE validator=?",
      last: "SELECT * FROM #{@table} WHERE validator=? ORDER BY height DESC",
      blank: "SELECT * FROM #{@table} WHERE id IS NULL ORDER BY ROWID, validator"
    }

  def last(validator_id) do
    call({:execute_step, :last, [validator_id]})
  end

  def count(validator_id) do
    call({:execute_step, :count, [validator_id]})
  end

  def blanks do
    call({:execute_fetch, :blank, []})
  end
end
