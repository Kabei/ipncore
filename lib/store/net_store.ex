defmodule NetStore do
  alias Ippan.Node
  alias Exqlite.Sqlite3NIF
  require Ippan.Node
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

  @app Mix.Project.config()[:app]
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

    load_nodes(db_ref)

    :ignore
  end

  defp load_nodes(db_ref) do
    if Node.total() == 0 do
      nodes = System.get_env("NODES")

      if is_nil(nodes) do
        IO.puts(IO.ANSI.red() <> "ERROR: variable NODES is missing" <> IO.ANSI.reset())
        System.halt(1)
      end

      pk = :persistent_term.get(:pubkey)
      net_pk = :persistent_term.get(:net_pubkey)
      default_port = Application.get_env(@app, :cluster)[:port]
      timestamp = :erlang.system_time(:millisecond)

      # registry cluster nodes
      nodes
      |> String.trim()
      |> String.split(~r/,|\|| /, trim: true)
      |> Enum.reduce([], fn x, acc ->
        acc ++ [String.split(x, "@", parts: 2)]
      end)
      |> Enum.each(fn [name_id, hostname] ->
        data =
          %Node{
            id: name_id,
            hostname: hostname,
            port: default_port,
            pubkey: pk,
            net_pubkey: net_pk,
            created_at: timestamp,
            updated_at: timestamp
          }
          |> Node.to_list()

        Node.insert(data)
      end)

      Sqlite.sync(db_ref)
    end
  end

  def terminate do
    db_ref = :persistent_term.get(@key_conn)
    Sqlite.release_statements(db_ref, @statements, :stmt)
    Sqlite3NIF.close(db_ref)
    :persistent_term.erase(@key_conn)
  end
end
