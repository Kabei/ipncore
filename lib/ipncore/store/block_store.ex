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
        PRIMARY KEY(round, creator_id, validator_id, height, hash)
      );
      """
    ],
    stmt: %{
      "fetch_round" => "SELECT hash FROM #{@table} WHERE round = ?1 ORDER BY creator ASC",
      "insert_vote" => "INSERT OR IGNORE INTO #{@table_bft} values(?1,?2,?3,?4,?5,?6,?7)",
      "fetch_votes" =>
        "SELECT * FROM #{@table_bft} WHERE round = ?1 ORDER BY creator_id ASC, validator_id ASC",
      "sum_votes" =>
        "SELECT SUM(votes), count(*) FROM #{@table_bft} WHERE round = ?1 and creator_id = ?2 and hash = ?3",
      "avg_round_time" => "SELECT AVG(timestamp) FROM #{@table} WHERE round = ?",
      "count_by_round" => "SELECT count(*) FROM #{@table} WHERE round = ?",
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
      lookup: "SELECT * FROM #{@table} WHERE height = ?",
      delete: "DELETE FROM #{@table} WHERE height = ?",
      count: "SELECT count(*) FROM #{@table} WHERE creator=?",
      last: "SELECT * FROM #{@table} WHERE creator=? ORDER BY height DESC"
    }

  def last(validator_id) do
    call({:execute_step, :last, [validator_id]})
  end

  def count(validator_id) do
    call({:execute_step, :count, [validator_id]})
  end

  def count_by_round(round) do
    call({:execute_step, "count_by_round", [round]})
  end

  def fetch_round(round) do
    call({:execute_fetch, "fetch_round", [round]})
  end

  def avg_round_time(round) do
    call({:execute_step, "avg_round_time", [round]})
  end

  def insert_vote(height, round, validator_id, creator_id, signature, hash, vote) do
    call(
      {:execute_step, "insert_vote",
       [height, round, validator_id, creator_id, signature, hash, vote]}
    )
  end

  def sum_votes(round, hash, creator) do
    call({:execute_step, "count_votes", [round, hash, creator]})
  end
end
