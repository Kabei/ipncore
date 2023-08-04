defmodule BlockStore do
  @table "block"
  @table_bft "bft"

  use Store.Sqlite2,
    base: :block,
    table: @table,
    mod: Ippan.Block,
    create: [
      """
      CREATE TABLE IF NOT EXISTS #{@table}(
        height BIGINT NOT NULL,
        creator BIGINT NOT NULL,
        hash BLOB NOT NULL,
        prev BLOB,
        hashfile BLOB,
        signature BLOB NOT NULL,
        round BIGINT NOT NULL,
        timestamp BIGINT NOT NULL,
        ev_count INTEGER DEFAULT 0,
        size BIGINT DEFAULT 0,
        error BOOLEAN DEFAULT FALSE,
        PRIMARY KEY(height, creator)
      ) WITHOUT ROWID;
      """,
      """
      CREATE TABLE IF NOT EXISTS #{@table_bft}(
        creator_id BIGINT NOT NULL,
        height BIGINT NOT NULL,
        validator_id BIGINT NOT NULL,
        round BIGINT NOT NULL,
        hash BLOB NOT NULL,
        PRIMARY KEY(creator_id, height, validator_id)
      ) WITHOUT ROWID;
      """
      # """
      # CREATE TABLE IF NOT EXISTS #{@table_candidates}(
      #   height BIGINT NOT NULL,
      #   creator BIGINT NOT NULL,
      #   hash BLOB NOT NULL,
      #   prev BLOB,
      #   hashfile BLOB,
      #   signature BLOB NOT NULL,
      #   round BIGINT NOT NULL,
      #   timestamp BIGINT NOT NULL,
      #   ev_count INTEGER DEFAULT 0,
      #   size BIGINT DEFAULT 0,
      #   error BOOLEAN DEFAULT FALSE,
      #   votes integer DEFAULT 0,
      #   PRIMARY KEY(height, creator),
      # ) WITHOUT ROWID;
      # """
    ],
    stmt: %{
      "last_block_by_creator" =>
        ~c"SELECT * FROM #{@table} WHERE creator = ? ORDER BY height DESC",
      "fetch_between" =>
        ~c"SELECT * FROM #{@table} WHERE creator = ?1 AND height BETWEEN ?2 AND ?3 ORDER BY height ASC",
      "fetch_uniques" =>
        ~c"SELECT creator, height FROM #{@table} WHERE round = ?1 ORDER BY creator ASC",
      "fetch_hash_round" => ~c"SELECT hash FROM #{@table} WHERE round = ?1 ORDER BY creator ASC",
      "insert_vote" => ~c"INSERT INTO #{@table_bft} values(?1,?2,?3,?4,?5)",
      "fetch_votes" =>
        ~c"SELECT * FROM #{@table_bft} WHERE round = ?1 ORDER BY creator_id ASC, validator_id ASC",
      "sum_votes" =>
        ~c"SELECT count(1) FROM #{@table_bft} WHERE round = ?1 and creator_id = ?2 and hash = ?3",
      "avg_round_time" => ~c"SELECT TRUNC(AVG(timestamp)) FROM #{@table} WHERE round = ?",
      "count_by_round" => ~c"SELECT count(1) FROM #{@table} WHERE round = ?",
      insert: ~c"INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6,?7,?8,?9,?10,?11)",
      lookup: ~c"SELECT * FROM #{@table} WHERE height = ?",
      delete: ~c"DELETE FROM #{@table} WHERE height = ?",
      count: ~c"SELECT count(1) FROM #{@table} WHERE creator=?",
      last: ~c"SELECT * FROM #{@table} WHERE creator=? ORDER BY height DESC"
    }

  def last(validator_id) do
    call({:step, :last, [validator_id]})
  end

  def fetch_between(creator_id, a, b) do
    call({:fetch, "fetch_between", [creator_id, a, b]})
  end

  def fetch_votes(round) do
    case call({:fetch, "fetch_votes", [round]}) do
      {:ok, res} -> res
      _ -> []
    end
  end

  def count(validator_id) do
    {_, [total]} = call({:step, :count, [validator_id]})
    total
  end

  def count_by_round(round) do
    {_, [total]} = call({:step, "count_by_round", [round]})
    total
  end

  def fetch_hash_round(round) do
    case call({:fetch, "fetch_hash_round", [round]}) do
      {:ok, res} -> res
      _ -> []
    end
  end

  def fetch_uniques(round) do
    case call({:fetch, "fetch_uniques", [round]}) do
      {:ok, res} -> Enum.map(res, fn [c, h] -> {c, h} end)
      _ -> []
    end
  end

  def avg_round_time(round) do
    call({:step, "avg_round_time", [round]})
  end

  def insert_vote(creator_id, height, validator_id, round, hash) do
    call({:step, "insert_vote", [creator_id, height, validator_id, round, hash]})
  end

  def sum_votes(round, hash, creator) do
    call({:step, "count_votes", [round, hash, creator]})
  end

  def last_block_by_creator(creator) do
    case call({:step, "last_block_by_creator", [creator]}) do
      {:row, data} -> data
      _ -> nil
    end
  end
end
