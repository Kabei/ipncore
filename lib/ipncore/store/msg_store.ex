defmodule MessageStore do
  @name "msg"

  @expiry_time :timer.hours(24)
  @every div(@expiry_time, Application.compile_env(:ipncore, :block_interval))

  @args %{
    "table" => "msg",
    "table_df" => "msgd",
    "table_hash" => "hashmap",
    "table_hashd" => "hashmapd",
    "expiry_time" => "#{@expiry_time}"
  }

  use Store.Sqlite2,
    base: :msg,
    table: @name,
    create: SQL.readFile!("lib/sql/msg.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/msg.stmt.sql", @args)

  def all_df do
    call({:fetch, "all_df", []})
  end

  def all_hash do
    call({:fetch, "all_hash", []})
  end

  def select(size, validator_id) do
    call({:fetch, "select", [size, validator_id]})
  end

  def select_df(size, validator_id) do
    call({:fetch, "select_df", [size, validator_id]})
  end

  def delete_all(nil), do: :ok
  def delete_all(-1), do: :ok

  def delete_all(rowid) do
    call({:step, "delete_all", [rowid]})
  end

  def delete_all_df(nil), do: :ok
  def delete_all_df(-1), do: :ok

  def delete_all_df(rowid) do
    call({:step, "delete_all_df", [rowid]})
  end

  def delete_all_df_approved(round) do
    call({:step, "delete_all_df_approved", [round]})
  end

  def select_all_df_approved(round) do
    call({:fetch, "select_all_df_approved", [round]})
  end

  def insert(params) do
    call({:step, :insert, params})
  end

  def insert_df(params) do
    call({:step, "insert_df", params})
  end

  def delete(hash) do
    call({:step, "delete_hash", [hash]})
  end

  def delete_df(hash) do
    call({:step, "delete_hash_df", [hash]})
  end

  def delete_df(timestamp, hash) do
    call({:step, "delete_df", [timestamp, hash]})
  end

  def approve_df(round, timestamp, hash) do
    call({:step, "approve_df", [round, timestamp, hash]})
  end

  def delete_not_approve_df(round) do
    call({:step, "delete_not_approve_df", [round]})
  end

  def delete_expiry(round, timestamp) do
    if round != 0 and rem(round, @every) == 0 do
      call({:step, "delete_expiry", [timestamp]})
      sync()
    end
  end
end
