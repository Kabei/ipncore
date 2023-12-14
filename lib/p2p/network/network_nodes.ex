defmodule Ippan.NetworkNodes do
  alias Ippan.{Network, Round, Validator}
  require Round
  require Validator
  require Sqlite

  @app Mix.Project.config()[:app]

  use Network,
    app: @app,
    name: :network,
    table: :nw,
    server: Ippan.NetworkNodes.Server,
    pubsub: :pubsub,
    topic: "network",
    opts: Application.compile_env(@app, :p2p_client),
    conn_opts: [retry: 2, reconnect: false],
    sup: Ippan.NetworkSup

  @impl Network
  def fetch(id) do
    db_ref = :persistent_term.get(:main_conn)
    Validator.get(id)
  end

  @impl Network
  def handle_request("get_rounds", data, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round_id = Map.get(data, "starts", 0)
    limit = Map.get(data, "limit", 100) |> min(200) |> trunc()
    offset = Map.get(data, "offset", 0)
    Round.fetch_all(round_id, limit, offset)
  end

  def handle_request("get_round", id, _state) when is_integer(id) do
    db_ref = :persistent_term.get(:main_conn)
    Round.get(id)
  end

  def handle_request("last_round", nil, _state) do
    db_ref = :persistent_term.get(:main_conn)

    case Round.last() do
      nil -> nil
      {id, hash} -> %{"id" => id, "hash" => hash}
    end
  end

  def handle_request("get_state", nil, _state) do
    %{round_id: round_id, round_hash: round_hash, block_id: block_id} =
      :sys.get_state(RoundManager)

    %{"id" => round_id, "hash" => round_hash, "block_id" => block_id}
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
