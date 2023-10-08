defmodule Ippan.ClusterNodes do
  require Logger
  alias Ippan.Wallet
  alias Ippan.{LocalNode, Network}
  require SqliteStore

  use Network,
    app: :ipncore,
    name: :cluster,
    table: :cnw,
    server: Ippan.ClusterNode.Server,
    pubsub: :cluster,
    topic: "cluster",
    opts: Application.compile_env(:ipncore, :p2p_client),
    conn_opts: [retry: 1, reconnect: false],
    sup: Ippan.ClusterSup

  def on_init(_) do
    nodes = System.get_env("NODES")

    if is_nil(nodes) do
      IO.puts(IO.ANSI.red() <> "ERROR: variable NODES is missing" <> IO.ANSI.reset())
      System.halt(1)
    end

    pk = :persistent_term.get(:pubkey)
    net_pk = :persistent_term.get(:net_pubkey)
    net_conn = :persistent_term.get(:net_conn)
    net_stmts = :persistent_term.get(:net_stmt)
    default_port = Application.get_env(:ipncore, :cluster)[:port]

    SqliteStore.step(net_conn, net_stmts, "delete_nodes", [])

    # registry cluster nodes
    String.split(nodes, ",", trim: true)
    |> Enum.reduce([], fn x, acc ->
      acc ++ [String.split(x, "@", parts: 2)]
    end)
    |> Enum.each(fn [name_id, hostname] ->
      data =
        %LocalNode{
          id: name_id,
          hostname: hostname,
          port: default_port,
          pubkey: pk,
          net_pubkey: net_pk
        }
        |> LocalNode.to_list()

      SqliteStore.step(net_conn, net_stmts, "insert_node", data)
    end)

    SqliteStore.sync(net_conn)
  end

  @impl Network
  def fetch(id) do
    SqliteStore.lookup_map(
      :cluster,
      :persistent_term.get(:net_conn),
      :persistent_term.get(:net_stmt),
      "get_node",
      id,
      LocalNode
    )
  end

  @impl Network
  def handle_request(
        "new_msg",
        [false, [hash, type, from, args, timestamp, nonce, msg_sig, size]],
        _state
      ) do
    case :ets.member(:msg, hash) do
      true ->
        ["error", "Already exists"]

      false ->
        dets = DetsPlux.get(:wallet)
        cache = DetsPlux.tx(:cache_nonce)

        case Wallet.update_nonce(dets, cache, from, nonce) do
          :error ->
            ["error", "Invalid nonce"]

          _ ->
            :ets.insert(:dmsg, {hash, [hash, type, from, args, timestamp, nonce, size]})
            :ets.insert(:msg, {hash, msg_sig})
        end

        %{"height" => :persistent_term.get(:height, 0)}
    end
  end

  def handle_request(
        "new_msg",
        [true, [hash, type, key | rest]],
        _state
      ) do
    case :ets.member(:msg, hash) do
      true ->
        ["error", "Already exists"]

      false ->
        height = :persistent_term.get(:height, 0)
        msg_key = {type, key}

        IO.puts("The same hash")

        case :ets.insert_new(:dhash, {msg_key, hash, height}) do
          true ->
            [from, args, timestamp, nonce, msg_sig, size] = rest

            dets = DetsPlux.get(:wallet)
            cache = DetsPlux.tx(:cache_nonce)

            IO.puts("The nonce")

            case Ippan.Wallet.update_nonce(dets, cache, "from", 1) do
              :error ->
                ["error", "Invalid nonce"]

              _ ->
                IO.puts("The insert")
                :ets.insert(:dmsg, {hash, [hash, type, key, from, args, timestamp, nonce, size]})
                :ets.insert(:msg, {hash, msg_sig})
            end

            IO.puts("The result")
            %{"height" => height}

          false ->
            ["error", "Deferred transaction already exists"]
        end
    end
  end

  def handle_request(_method, _data, _state), do: ["error", "Not found"]

  @impl Network
  def handle_message(_event, _data, _state), do: :ok
end
