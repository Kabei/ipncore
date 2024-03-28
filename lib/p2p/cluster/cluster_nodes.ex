defmodule Ippan.ClusterNodes do
  require Ippan.Round
  alias Ippan.Round
  alias Ippan.{Node, Network}
  require Ippan.{Node, TxHandler, Round}
  require BalanceStore
  require Sqlite
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
    Node.get(id)
  end

  @impl Network
  def exists?(id) do
    db_ref = :persistent_term.get(:local_conn)
    Node.exists?(id)
  end

  @impl Network
  def handle_request(
        "new_msg",
        [false, body, return],
        _state
      ) do
    status = :persistent_term.get(:status, nil)

    if status == :synced do
      Mempool.regular(body, return)
    else
      {:error, "Node waiting for synchronization"}
    end
  end

  def handle_request(
        "new_msg",
        [true, body, return],
        _state
      ) do
    status = :persistent_term.get(:status, nil)

    if status == :synced do
      Mempool.deferred(body, return)
    else
      {"error", "Node waiting for synchronization"}
    end
  end

  def handle_request("height", _params, _state) do
    db_ref = :persistent_term.get(:main_conn)

    snap = Snapshot.last()

    Round.last() |> Map.merge(%{"snapshot" => snap})
  end

  # def handle_request("last_round", _params, _state) do
  #   db_ref = :persistent_term.get(:main_conn)
  #   Round.last()
  # end

  def handle_request("get_round", id, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round = Round.get(id) || %{}

    case Sqlite.fetch("get_jackpot") do
      nil ->
        Map.put(round, "jackpot", {nil, 0})

      [winner, amount] ->
        Map.put(round, "jackpot", {winner, amount})
    end
  end

  def handle_request("get_rounds", params, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round_id = Map.get(params, "starts", 0)
    limit = Map.get(params, "limit", 50) |> min(200) |> trunc()
    offset = Map.get(params, "offset", 0)

    case Sqlite.fetch_all("get_rounds", [round_id, limit, offset]) do
      [] ->
        []

      data ->
        Enum.map(data, fn x ->
          Round.list_to_map(x)
        end)
    end
  end

  def handle_request(_method, _data, _state), do: {"error", "Not found"}

  @impl Network
  def handle_message(event = "node.join", data, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.insert(Node.to_list(data)) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.update", data = %{"data" => fields, "id" => id}, %{
        "id" => node_id
      }) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.update(fields, id) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.leave", id, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:local_conn)

    if Node.delete(id) == :done do
      broadcast_except(%{"event" => event, "data" => id}, [node_id])
    end
  end

  def handle_message(_event, _data, _state), do: :ok
end
