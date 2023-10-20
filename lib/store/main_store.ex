defmodule MainStore do
  alias Exqlite.Sqlite3NIF
  require Sqlite

  @version 0

  @creations %{
    "assets" => SQL.readFile!("lib/sql/assets.sql"),
    "blockchain" => SQL.readFile!("lib/sql/blockchain.sql"),
    "dns" => SQL.readFile!("lib/sql/dns.sql"),
    "main" => SQL.readFile!("lib/sql/main.sql")
  }

  @statements SQL.readStmtFile!("lib/sql/main.stmt.sql")

  @alter []

  # databases
  @attaches %{
    "account" => "accounts.db",
    "assets" => "assets.db",
    "dns" => "dns.db",
    "blockchain" => "blockchain.db"
  }

  @name "main"
  @filename "main.db"
  @key_conn :main_conn

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [args]}
    }
  end

  def init(_) do
    filename = Path.join(:persistent_term.get(:store_dir), @filename)

    {:ok, db_ref} = Sqlite.open_setup(@name, filename, @creations, @attaches)
    # execute alter tables if exists new version
    :ok = Sqlite.check_version(db_ref, @alter, @version)
    # prepare statements
    Sqlite.prepare_statements(db_ref, @statements, :stmt)
    Sqlite.begin(db_ref)
    # put in global conn and statements
    :persistent_term.put(@key_conn, db_ref)

    Platform.start()

    :ignore
  end

  def terminate do
    db_ref = :persistent_term.get(@key_conn)
    Sqlite.release_statements(db_ref, @statements, :stmt)
    Sqlite3NIF.close(db_ref)
    :persistent_term.erase(@key_conn)
  end
end
