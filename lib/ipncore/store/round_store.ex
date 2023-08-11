defmodule RoundStore do
  @table "round"

  @args %{
    "round" => "round",
    "jackpot" => "jackpot",
    "snapshot" => "snapshot"
  }

  use Store.Sqlite2,
    base: :round,
    table: @table,
    mod: Ippan.Round,
    create: SQL.readFile!("lib/sql/round.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/round.stmt.sql", @args)

  def last do
    call({:step, "last", []})
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
    {_, [total]} = call({:step, "total", []})
    total
  end
end
