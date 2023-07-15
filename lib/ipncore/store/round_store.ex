defmodule RoundStore do
  @table "round"
  @table_jackpot "jackpot"

  use Store.Sqlite,
    base: :round,
    table: @table,
    mod: Ippan.Round,
    create: ["
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hash BLOB NOT NULL,
      prev BLOB,
      blocks BIGINT NOT NULL,
      timestamp BIGINT NOT NULL
    ) WITHOUT ROWID;
    ", "
    CREATE TABLE IF NOT EXISTS #{@table_jackpot}(
      id BIGINT PRIMARY KEY NOT NULL,
      winner_id BLOB,
      PRIMARY KEY(id, winner_id)
    ) WITHOUT ROWID;
    "],
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      last: "SELECT * FROM #{@table} ORDER BY id DESC LIMIT 1",
      delete: "DELETE FROM #{@table} WHERE id = ?",
      insert_winner: "INSERT INTO #{@table_jackpot} values(?, ?)",
      has_winner: "SELECT 1 FROM #{@table_jackpot} WHERE id = ?"
    }

  def last do
    call({:execute_step, :last, []})
  end

  def last_id do
    case RoundStore.last() do
      {:row, [round_id | _]} -> round_id
      _ -> 0
    end
  end

  def insert_winner(round_id, winner_id) do
    call({:execute_step, :insert_winner, [round_id, winner_id]})
  end

  def has_winner?(round_id) do
    {:row, [1]} == call({:execute_step, :insert_winner, [round_id]})
  end
end
