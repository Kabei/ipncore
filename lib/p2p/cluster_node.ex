defmodule Ippan.ClusterNode do
  alias Ippan.Network

  use Network,
    app: :ipncore,
    name: :cluster,
    table: :cnw,
    pubsub: :cluster,
    topic: "cluster",
    opts: Application.compile_env(:ipncore, :p2p_client),
    sup: Ippan.ClusterSup

  @impl Network
  def fetch(id) do
    SqliteStore.lookup_map(
      :cluster,
      :persistent_term.get(:asset_conn),
      :persistent_term.get(:asset_stmt),
      "get_node",
      id,
      LocalCluster
    )
  end

  @impl Network
  def handle_request(_method, _data, _state), do: "not found"

  @impl Network
  # def handle_message(
  #       "msg",
  #       _from,
  #       [hash, timestamp, type, from, args, size] = data,
  #       %{conn: conn, stmts: stmts, dets: dets} = state
  #     ) do
  # end

  def handle_message(_event, _data, _state), do: :ok
end
