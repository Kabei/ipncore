defmodule NetStore do
  use GenServer
  alias Exqlite.Sqlite3NIF
  require SqliteStore

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
  @key_stmt :net_stmt

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_) do
    filename = Path.join(:persistent_term.get(:store_dir), @filename)

    {:ok, conn} = SqliteStore.open_setup(@name, filename, @creations, @attaches)
    # execute alter tables if exists new version
    :ok = SqliteStore.check_version(conn, @alter, @version)
    # prepare statements
    {:ok, stmts} = SqliteStore.prepare_statements(conn, @statements)
    # put in global conn and statements
    :persistent_term.put(@key_conn, conn)
    :persistent_term.put(@key_stmt, stmts)
    # begin tx
    SqliteStore.begin(conn)

    {:ok, %{}, :hibernate}
  end

  def commit(conn) do
    :gen_server.call(__MODULE__, {:commit, conn}, :infinity)
  end

  @impl true
  def handle_call({:commit, conn}, _from, state) do
    SqliteStore.commit(conn)
    SqliteStore.begin(conn)
    {:reply, :ok, state, :hibernate}
  end

  @impl true
  def terminate(_reason, _state) do
    conn = :persistent_term.get(@key_conn)
    stmts = :persistent_term.get(@key_stmt)
    SqliteStore.release_statements(conn, stmts)
    Sqlite3NIF.close(conn)
    :persistent_term.erase(@key_conn)
    :persistent_term.erase(@key_stmt)
    :ok
  end
end
