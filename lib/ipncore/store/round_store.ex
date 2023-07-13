defmodule RoundStore do
  @table "round"

  use Store.Sqlite,
    base: :round,
    table: @table,
    mod: Ippan.Round,
    create: """
    CREATE TABLE IF NOT EXISTS #{@table}(
      id BIGINT PRIMARY KEY NOT NULL,
      hash BLOB NOT NULL,
      prev BLOB,
      blocks BIGINT NOT NULL,
      timestamp BIGINT NOT NULL
    ) WITHOUT ROWID;
    """,
    stmt: %{
      insert: "INSERT INTO #{@table} values(?1,?2,?3,?4,?5)",
      lookup: "SELECT * FROM #{@table} WHERE id = ?",
      exists: "SELECT 1 FROM #{@table} WHERE id = ?",
      last: "SELECT * FROM #{@table} ORDER BY id DESC LIMIT 1",
      delete: "DELETE FROM #{@table} WHERE id = ?"
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
end
