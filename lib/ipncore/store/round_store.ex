defmodule RoundStore do
  @table "round"
  @table_jackpot "jackpot"
  @table_snapshot "snapshot"

  use Store.Sqlite2,
    base: :round,
    table: @table,
    mod: Ippan.Round,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hash BLOB NOT NULL,
      prev BLOB,
      blocks BIGINT NOT NULL,
      timestamp BIGINT NOT NULL,
      vsn TINTYINT NOT NULL
    ) WITHOUT ROWID;
    ", "
    CREATE TABLE IF NOT EXISTS #{@table_jackpot}(
      round_id BIGINT NOT NULL,
      winner_id BLOB,
      amount BIGINT DEFAULT 0,
      PRIMARY KEY(round_id, winner_id)
    ) WITHOUT ROWID;
    ", "
    CREATE TABLE IF NOT EXISTS #{@table_snapshot}(
      round_id BIGINT PRIMARY KEY NOT NULL,
      hash BLOB NOT NULL,
      size BIGINT NOT NULL
    ) WITHOUT ROWID;"],
    stmt: %{
      "has_winner" => ~c"SELECT 1 FROM #{@table_jackpot} WHERE round_id = ?",
      "insert_winner" => ~c"INSERT INTO #{@table_jackpot} values(?, ?, ?)",
      "insert_snap" => ~c"INSERT INTO #{@table_snapshot} values(?, ?, ?)",
      insert: ~c"INSERT INTO #{@table} values(?1,?2,?3,?4,?5,?6)",
      lookup: ~c"SELECT * FROM #{@table} WHERE id = ?",
      exists: ~c"SELECT 1 FROM #{@table} WHERE id = ?",
      last: ~c"SELECT * FROM #{@table} ORDER BY id DESC LIMIT 1",
      delete: ~c"DELETE FROM #{@table} WHERE id = ?",
      total: ~c"SELECT COUNT(1) FROM #{@table}"
    }

  def last do
    call({:step, :last, []})
  end

  def last_id do
    case RoundStore.last() do
      {:row, [round_id | _]} -> round_id
      _ -> 0
    end
  end

  def insert_winner(round_id, winner_id, amount) do
    call({:step, "insert_winner", [round_id, winner_id, amount]})
  end

  def has_winner?(round_id) do
    {:row, [1]} == call({:step, "has_winner", [round_id]})
  end

  def total do
    {_, [total]} = call({:step, :total, []})
    total
  end
end
