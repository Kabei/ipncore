defmodule Ippan.NetworkNodes do
  alias Ippan.{Block, Network, Round, Validator}
  require Round
  require Validator
  require Sqlite

  @app Mix.Project.config()[:app]

  use Network,
    app: @app,
    name: :network,
    table: :nw,
    bag: :nwb,
    via: :client,
    server: Ippan.NetworkNodes.Server,
    pubsub: :pubsub,
    topic: "network",
    opts: Application.compile_env(@app, :p2p_client),
    conn_opts: [retry: 3, reconnect: true],
    sup: Ippan.NetworkSup

  @impl Network
  def fetch(id) do
    db_ref = :persistent_term.get(:main_conn)
    Validator.get(id)
  end

  @impl Network
  def exists?(id) do
    db_ref = :persistent_term.get(:main_conn)
    Validator.exists?(id)
  end

  @impl Network
  def handle_request("get_rounds", data, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round_id = Map.get(data, "starts", 0)
    limit = Map.get(data, "limit", 50) |> min(200) |> trunc()
    offset = Map.get(data, "offset", 0)
    Round.fetch_all(round_id, limit, offset)
  end

  def handle_request("get_round", id, _state) when is_integer(id) do
    db_ref = :persistent_term.get(:main_conn)

    case Round.get(id) do
      nil ->
        GenServer.call(RoundManager, {:round, id}, 10_000)

      r ->
        r
    end
  end

  def handle_request("last_round", _params, _state) do
    db_ref = :persistent_term.get(:main_conn)

    Round.last()
  end

  def handle_request(_method, _data, _state), do: {"error", "Not found"}

  @impl Network
  def handle_message("round_msg", data, %{id: from}) when is_map(data) do
    GenServer.cast(RoundManager, {"round_msg", Round.from_remote(data), from})
  end

  def handle_message("round_ok", data, %{id: from}) when is_map(data) do
    GenServer.cast(RoundManager, {"round_ok", data, from})
  end

  def handle_message("round_off", data, %{id: from}) when is_map(data) do
    GenServer.cast(RoundManager, {"round_off", Round.from_remote(data), from})
  end

  def handle_message("block_msg", data, %{id: from}) when is_map(data) do
    GenServer.cast(RoundManager, {"block_msg", Block.from_remote(data), from})
  end

  def handle_message(event, _from, data) do
    IO.inspect("network not match #{event} #{inspect(data)}")
    :ok
  end
end
