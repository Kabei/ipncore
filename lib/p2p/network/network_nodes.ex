defmodule Ippan.NetworkNodes do
  alias Ippan.Round
  alias Ippan.Validator
  alias Ippan.Network
  require SqliteStore

  use Network,
    app: :ipncore,
    name: :network,
    table: :nw,
    server: Ippan.NetworkNodes.Server,
    pubsub: :network,
    topic: "network",
    opts: Application.compile_env(:ipncore, :p2p_client),
    conn_opts: [retry: 2, reconnect: false],
    sup: Ippan.NetworkSup

  @impl Network
  def fetch(id) do
    SqliteStore.lookup_map(
      :validator,
      :persistent_term.get(:asset_conn),
      :persistent_term.get(:asset_stmt),
      "get_validator",
      id,
      Validator
    )
  end

  @impl Network
  def handle_request("get_rounds", data, _state) do
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)
    limit = Map.get(data, "limit", 200) |> min(200) |> trunc()
    offset = Map.get(data, "offset", 0)
    SqliteStore.fetch_all(conn, stmts, "get_rounds", limit, offset)
  end

  def handle_request("get_round", id, _state) when is_integer(id) do
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)

    case SqliteStore.fetch(conn, stmts, "get_round", [id]) do
      nil -> nil
      data -> Round.list_to_map(data)
    end
  end

  def handle_request("last_round", nil, _state) do
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)

    case SqliteStore.fetch(conn, stmts, "last_round", []) do
      nil -> nil
      [id, hash] -> %{"id" => id, "hash" => hash}
    end
  end

  def handle_request(_method, _data, _state), do: ["error", "Not found"]

  @impl Network
  def handle_message("msg_round", data, %{id: from}) when is_map(data) do
    send(RoundManager, {"msg_round", Round.from_remote(data), from})
  end

  def handle_message("msg_block", data, %{id: from}) when is_map(data) do
    send(RoundManager, {"msg_block", data, from})
  end

  def handle_message(_event, _from, _data), do: :ok
end
