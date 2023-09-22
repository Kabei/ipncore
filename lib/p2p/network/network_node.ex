defmodule Ippan.NetworkNode do
  alias Ippan.Round
  alias Ippan.Validator
  alias Ippan.Network
  require SqliteStore

  # @max_block_data_size Application.compile_env(:ipncore, :max_block_data_size)

  use Network,
    app: :ipncore,
    name: :network,
    table: :nw,
    server: Ippan.NetworkNode.Server,
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
  def handle_message("msg_round", %{"blocks" => blocks} = data, %{id: from}) when is_map(data) do
    blocks =
      Enum.reduce(blocks, [], fn b, acc ->
        block =
          MapUtil.to_atoms(b, ~w(hash height creator prev size hashfile timestamp count vsn))

        acc ++ [block]
      end)

    msg_round =
      data
      |> MapUtil.to_atoms(~w(id creator hash prev signature))
      |> Map.put(:blocks, blocks)

    send(RoundManager, {"msg_round", msg_round, from})
  end

  def handle_message("msg_block", data, %{id: from}) when is_map(data) do
    send(RoundManager, {"msg_block", data, from})
  end

  def handle_message(_event, _from, _data), do: :ok
end
