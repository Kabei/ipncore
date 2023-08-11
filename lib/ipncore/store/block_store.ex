defmodule BlockStore do
  @table "block"
  @table_bft "bft"

  @args %{
    "block" => @table,
    "msg_block" => @table_bft
  }

  use Store.Sqlite2,
    base: :block,
    table: @table,
    create: SQL.readFile!("lib/sql/block.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/block.stmt.sql", @args)

  def last(validator_id) do
    call({:step, "last", [validator_id]})
  end

  def count(validator_id) do
    {_, [total]} = call({:step, "count", [validator_id]})
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

  def insert_bft(validator_id, round, creator_id, height, msg) do
    cast(
      {:step, "insert_bft",
       [
         validator_id,
         round,
         creator_id,
         height,
         :erlang.term_to_binary(msg),
         :os.system_time(:millisecond)
       ]}
    )
  end

  def fetch_bft(round) do
    case call({:fetch, "fetch_bft", [round]}) do
      {:ok, res} -> res
      _ -> []
    end
  end

  def delete_bft(round) do
    cast({:step, "delete_bft", [round]})
  end
end
