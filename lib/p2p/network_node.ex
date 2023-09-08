defmodule Ippan.NetworkNode do
  alias Ippan.Validator
  alias Ippan.Network
  require SqliteStore

  # @max_block_data_size Application.compile_env(:ipncore, :max_block_data_size)

  use Network,
    app: :ipncore,
    name: :network,
    table: :nw,
    pubsub: :network,
    topic: "network",
    opts: Application.compile_env(:ipncore, :p2p_client),
    conn_opts: [retry: 3],
    sup: Ippan.NetworkSup

  @impl Network
  def fetch(id) do
    SqliteStore.lookup_map(
      :valdiator,
      :persistent_term.get(:net_conn),
      :persistent_term.get(:net_stmt),
      "get_validator",
      id,
      Validator
    )
  end

  @impl Network
  def handle_request("get_msg_rounds", data, _state) do
    conn = :persistent_term.get(:net_conn)
    stmts = :persistent_term.get(:net_stmt)
    limit = Map.get(data, "limit", 100) |> min(100) |> trunc()
    offset = Map.get(data, "offset", 0)
    SqliteStore.fetch_all(conn, stmts, "get_msg_rounds", limit, offset)
  end

  def handle_request(_method, _data, _state), do: "not found"

  @impl Network
  def handle_message("msg_round", data, %{id: from}) do
    send(RoundManager, {"msg_round", data, from})
  end

  def handle_message("msg_block", data, %{id: from}) do
    send(RoundManager, {"msg_block", data, from})
  end

  def handle_message(_event, _from, _data), do: :ok
end
