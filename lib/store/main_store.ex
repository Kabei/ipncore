defmodule MainStore do
  alias Exqlite.Sqlite3NIF
  require SqliteStore

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
    "assets" => "assets.db",
    "dns" => "dns.db",
    "blockchain" => "blockchain.db"
  }

  @name "main"
  @filename "main.db"
  @key_conn :asset_conn
  @key_stmt :asset_stmt

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [args]}
    }
  end

  def init(_) do
    filename = Path.join(:persistent_term.get(:store_dir), @filename)

    {:ok, conn} = SqliteStore.open_setup(@name, filename, @creations, @attaches)
    # execute alter tables if exists new version
    :ok = SqliteStore.check_version(conn, @alter, @version)
    # prepare statements
    {:ok, stmts} = SqliteStore.prepare_statements(conn, @statements)
    SqliteStore.begin(conn)
    # put in global conn and statements
    :persistent_term.put(@key_conn, conn)
    :persistent_term.put(@key_stmt, stmts)

    Platform.start()

    :ignore
  end

  @spec terminate :: :ok
  def terminate do
    conn = :persistent_term.get(@key_conn)
    stmts = :persistent_term.get(@key_stmt)
    SqliteStore.release_statements(conn, stmts)
    Sqlite3NIF.close(conn)
    :persistent_term.erase(@key_conn)
    :persistent_term.erase(@key_stmt)
    :ok
  end
end
