defmodule NetStore do
  alias Exqlite.Sqlite3NIF
  require Sqlite

  @version 0

  @creations %{
    "network" => SQL.readFile!("lib/sql/network.sql")
  }

  @statements SQL.readStmtFile!("lib/sql/network.stmt.sql")

  # SQL.readStmtFile!("lib/sql/assets_alter.stmt.sql")
  @alter []

  # databases
  @attaches %{}

  @name "network"
  @filename "network.db"
  @key_conn :net_conn

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [args]}
    }
  end

  def init(_args) do
    filename = Path.join(:persistent_term.get(:store_dir), @filename)

    {:ok, db_ref} = Sqlite.open_setup(@name, filename, @creations, @attaches)
    # execute alter tables if exists new version
    :ok = Sqlite.check_version(db_ref, @alter, @version)
    # prepare statements
    Sqlite.prepare_statements(db_ref, @statements, :stmt)
    # put in global conn and statements
    :persistent_term.put(@key_conn, db_ref)
    # begin tx
    Sqlite.begin(db_ref)

    :ignore
  end

  def terminate do
    db_ref = :persistent_term.get(@key_conn)
    Sqlite.release_statements(db_ref, @statements, :stmt)
    Sqlite3NIF.close(db_ref)
    :persistent_term.erase(@key_conn)
  end
end
