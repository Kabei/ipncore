defmodule Ippan.Func.Env do
  require SqliteStore

  def set(%{conn: conn, stmts: stmts, timestamp: timestamp}, name, value) do
    EnvStore.put(conn, stmts, name, value, timestamp)
  end

  def delete(%{conn: conn, stmts: stmts}, name) do
    EnvStore.delete(conn, stmts, name)
  end
end
