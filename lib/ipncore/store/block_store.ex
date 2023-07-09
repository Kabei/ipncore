defmodule BlockStore do
  @table "block"
  @table_bft "bft"

  use Store.Sqlite,
    base: :block,
    table: @table,
    mod: Ippan.Block,
    create: [
      """
      CREATE TABLE IF NOT EXISTS #{@table}(
        height BIGINT,
        creator BIGINT NOT NULL,
        hash BLOB,
        prev BLOB,
        hashfile BLOB NOT NULL,
        signature BLOB NOT NULL,
        round BIGINT NOT NULL,
        timestamp BIGINT NOT NULL,
        ev_count BIGINT DEFAULT 0,
        size BIGINT NOT NULL,
        vsn SMALLINT NOT NULL,
        PRIMARY KEY(height, creator)
      );
      """,
      """
      CREATE TABLE IF NOT EXISTS #{@table_bft}(
        height BIGINT NOT NULL,
        round BIGINT NOT NULL,
        creator_id BIGINT NOT NULL,
        validator_id BIGINT NOT NULL,
        hash BLOB NOT NULL,
        signature BLOB NOT NULL,
        votes INTEGER NOT NULL,
        PRIMARY KEY(height, round, creator_id, validator_id, hash)
      );
      """
    ],
    stmt: %{
      "fetch_round" => "SELECT hash FROM #{@table} WHERE round = ?1",
      "insert_vote" => "INSERT OR IGNORE INTO #{@table_bft} values(?1,?2,?3,?4,?5,?6,?7)",
      "fetch_votes" => "SELECT * FROM #{@table_bft} WHERE round = ?1",
      "sum_votes" => "SELECT SUM(votes), count(*) FROM #{@table_bft} WHERE round = ?1 and hash = ?2",
      "last_prev" => "SELECT prev FROM #{@table} WHERE creator = ?1 ORDER BY height LIMIT 1",
      insert:
        "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
      lookup: "SELECT * FROM #{@table} WHERE height = ?",
      lookup_hash: "SELECT * FROM #{@table} WHERE hash = ? LIMIT 1",
      exists: "SELECT 1 FROM #{@table} WHERE height = ?",
      delete: "DELETE FROM #{@table} WHERE height = ?",
      count: "SELECT count(*) FROM #{@table} WHERE creator=?",
      last: "SELECT * FROM #{@table} WHERE creator=? ORDER BY height DESC"
      # blank: "SELECT * FROM #{@table} WHERE id IS NULL ORDER BY ROWID, validator"
    }

  def last(validator_id) do
    call({:execute_step, :last, [validator_id]})
  end

  def last_prev(validator_id) do
    call({:execute_step, "last_prev", [validator_id]})
  end

  def count(validator_id) do
    call({:execute_step, :count, [validator_id]})
  end

  def fetch_round(round) do
    call({:execute_fetch, "fetch_round", [round]})
  end

  def insert_vote(height, round, validator_id, creator_id, signature, hash, vote) do
    call(
      {:execute_step, "insert_vote", [height, round, validator_id, creator_id, signature, hash, vote]}
    )
  end

  def sum_votes(round) do
    call({:execute_step, "count_votes", [round]})
  end
end
