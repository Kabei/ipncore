defmodule Ippan.Funx.Env do
  alias Phoenix.PubSub
  require SqliteStore

  def set(%{conn: conn, stmts: stmts, timestamp: timestamp}, name, value) do
    :done = EnvStore.put(conn, stmts, name, value, timestamp)
    event = %{"event" => "env.new", "data" => %{"name" => name, "value" => value}}
    PubSub.broadcast(:cluster, "env", event)
  end

  def delete(%{conn: conn, stmts: stmts}, name) do
    EnvStore.delete(conn, stmts, name)
    event = %{"event" => "env.delete", "data" => name}
    PubSub.broadcast(:cluster, "env", event)
  end
end
