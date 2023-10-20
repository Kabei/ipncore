defmodule Ippan.Funx.Env do
  alias Phoenix.PubSub
  require Sqlite

  def set(_, name, value) do
    db_ref = :persistent_term.get(:main_conn)
    EnvStore.put(db_ref, name, value)
    event = %{"event" => "env.new", "data" => %{"name" => name, "value" => value}}
    PubSub.broadcast(:pubsub, "env", event)
  end

  def delete(_, name) do
    db_ref = :persistent_term.get(:main_conn)
    EnvStore.delete(db_ref, name)
    event = %{"event" => "env.delete", "data" => name}
    PubSub.broadcast(:pubsub, "env", event)
  end
end
