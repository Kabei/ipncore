defmodule DomainStore do
  @table "domain"
  @args %{"table" => @table}
  @every div(:timer.hours(24), Application.compile_env(:ipncore, :block_interval))

  use Store.Sqlite2,
    base: :domain,
    table: @table,
    mod: Ippan.Domain,
    create: SQL.readFile!("lib/sql/domain.sql", @args),
    stmt: SQL.readFileStmt!("lib/sql/domain.stmt.sql", @args)

  def renew(name, account_id, millis, timestamp) do
    call({:changes, :renew, [name, account_id, millis, timestamp]})
  end

  def delete_expiry(round, timestamp) do
    if round != 0 and rem(round, @every) == 0 do
      call({:step, "delete_expiry", [timestamp]})
      sync()
    end
  end
end
