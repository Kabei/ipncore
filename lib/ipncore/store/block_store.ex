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
        PRIMARY KEY(creator_id, height, validator_id)
      );
      """
    ],
    stmt: %{
      "fetch_between" =>
        "SELECT hash FROM #{@table} WHERE creator = ?1 AND height BETWEEN ?2 AND ?3 ORDER BY height ASC",
      "fetch_round" => "SELECT hash FROM #{@table} WHERE round = ?1 ORDER BY creator ASC",
      "insert_vote" => "INSERT INTO #{@table_bft} values(?1,?2,?3,?4,?5,?6,?7)",
      "fetch_votes" =>
        "SELECT * FROM #{@table_bft} WHERE round = ?1 ORDER BY creator_id ASC, validator_id ASC",
      "sum_votes" =>
        "SELECT SUM(votes), count(1) FROM #{@table_bft} WHERE round = ?1 and creator_id = ?2 and hash = ?3",
      "avg_round_time" => "SELECT TRUNC(AVG(timestamp)) FROM #{@table} WHERE round = ?",
      "count_by_round" => "SELECT count(1) FROM #{@table} WHERE round = ?",
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
      lookup: "SELECT * FROM #{@table} WHERE height = ?",
      delete: "DELETE FROM #{@table} WHERE height = ?",
      count: "SELECT count(1) FROM #{@table} WHERE creator=?",
      last: "SELECT * FROM #{@table} WHERE creator=? ORDER BY height DESC"
    }

  def last(validator_id) do
    call({:execute_step, :last, [validator_id]})
  end

  def fetch_between(creator_id, a, b) do
    call({:execute_fetch, "fetch_between", [creator_id, a, b]})
  end

  def count(validator_id) do
    {_, [total]} = call({:execute_step, :count, [validator_id]})
    total
  end

  def count_by_round(round) do
    {_, [total]} = call({:execute_step, "count_by_round", [round]})
    total
  end

  def fetch_round(round) do
    call({:execute_fetch, "fetch_round", [round]})
  end

  def avg_round_time(round) do
    call({:execute_step, "avg_round_time", [round]})
  end

  def insert_vote(height, round, validator_id, creator_id, hash, signature, vote) do
    call(
      {:execute_step, "insert_vote",
       [height, round, validator_id, creator_id, signature, hash, vote]}
    )
  end

  def sum_votes(round, hash, creator) do
    call({:execute_step, "count_votes", [round, hash, creator]})
  end
end
