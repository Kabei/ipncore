defmodule Ippan.ClusterNodes do
  alias Ippan.Round
  alias Ippan.{Node, Network}
  require BalanceStore

  require Logger

  @app Mix.Project.config()[:app]

  use Network,
    app: @app,
    name: :cluster,
    table: :cnw,
    bag: :cnb,
    via: :server,
    server: Ippan.ClusterNode.Server,
    pubsub: :pubsub,
    topic: "cluster",
    opts: Application.compile_env(@app, :p2p_client),
    conn_opts: [retry: 1, reconnect: false],
    sup: Ippan.ClusterSup

  def on_init(_) do
    :ok
  end

  @impl Network
  def fetch(id) do
    db_ref = :persistent_term.get(:local_conn)
    Node.get(db_ref, id)
  end

  @impl Network
  def exists?(id) do
    db_ref = :persistent_term.get(:local_conn)
    Node.exists?(db_ref, id)
  end

  @impl Network
  # {_hash, _type_id, _from, _nonce, _args, _size, signature}
  def handle_request("tx", %{"body" => body_and_signature, "tx" => transaction}, _state) do
    if :persistent_term.get(:status, nil) == :synced do
      pool = MemPool.get()
      MemPool.add(pool, transaction, body_and_signature)
    else
      {"error", "Node waiting for synchronization"}
    end
  end

  def handle_request("height", _params, _state) do
    db_ref = :persistent_term.get(:main_conn)

    snap = Snapshot.last()

    Round.last(db_ref) |> Map.merge(%{"snapshot" => snap})
  end

  # def handle_request("last_round", _params, _state) do
  #   db_ref = :persistent_term.get(:main_conn)
  #   Round.last()
  # end

  def handle_request("get_round", id, _state) do
    db_ref = :persistent_term.get(:main_conn)
    Round.get(db_ref, id) || %{}
  end

  def handle_request("get_rounds", params, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round_id = Map.get(params, "starts", 0)
    limit = Map.get(params, "limit", 50) |> min(200) |> trunc()
    offset = Map.get(params, "offset", 0)

    case Sqlite.fetch_all(db_ref, "get_rounds", [round_id, limit, offset]) do
      [] -> []
      data -> Enum.map(data, &Round.list_to_map(&1))
    end
  end

  def handle_request(_method, _data, _state), do: {"error", "Not found"}

  @impl Network
  def handle_message(event = "node.join", data, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.insert(db_ref, data) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.update", data = %{"data" => fields, "id" => id}, %{
        "id" => node_id
      }) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.update(db_ref, fields, id) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.leave", id, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.delete(db_ref, id) == :done do
      broadcast_except(%{"event" => event, "data" => id}, [node_id])
    end
  end

  def handle_message(_event, _data, _state), do: :ok
end
