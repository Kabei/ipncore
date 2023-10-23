defmodule Ippan.ClusterNodes do
  require Ippan.Round
  alias Ippan.Round
  alias Ippan.{Node, Network, TxHandler, Wallet}
  require Ippan.{Node, TxHandler, Round}
  require BalanceStore
  require Sqlite
  require Logger

  use Network,
    app: :ipncore,
    name: :cluster,
    table: :cnw,
    server: Ippan.ClusterNode.Server,
    pubsub: :pubsub,
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
    db_ref = :persistent_term.get(:net_conn)
    default_port = Application.get_env(:ipncore, :cluster)[:port]

    Node.delete_all()

    # registry cluster nodes
    String.split(nodes, ",", trim: true)
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
          net_pubkey: net_pk
        }
        |> Node.to_list()

      Node.insert(data)
    end)

    Sqlite.sync(db_ref)
  end

  @impl Network
  def fetch(id) do
    db_ref = :persistent_term.get(:net_conn)
    Node.get(id)
  end

  @impl Network
  def handle_request(
        "new_msg",
        [false, [hash, type, from, nonce, args, msg_sig, size], return],
        _state
      ) do
    case :ets.member(:msg, hash) do
      true ->
        ["error", "Already exists"]

      false ->
        dets = DetsPlux.get(:nonce)
        cache = DetsPlux.tx(dets, :cache_nonce)

        IO.puts("The nonce")

        case Wallet.update_nonce(dets, cache, from, nonce) do
          :error ->
            ["error", "Invalid nonce"]

          _ ->
            IO.puts("The check return")
            TxHandler.check_return!()

            IO.puts("The insert")
            :ets.insert(:dmsg, {hash, [hash, type, from, nonce, args, size]})
            :ets.insert(:msg, {hash, msg_sig})
            IO.puts("The result")
            %{"height" => :persistent_term.get(:height, 0)}
        end
    end
  end

  def handle_request(
        "new_msg",
        [true, [hash, type, key | rest], return],
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
            [from, nonce, args, msg_sig, size] = rest

            dets = DetsPlux.get(:wallet)
            cache = DetsPlux.tx(:wallet, :cache_nonce)

            IO.puts("The nonce")

            case Ippan.Wallet.update_nonce(dets, cache, from, nonce) do
              :error ->
                ["error", "Invalid nonce"]

              _ ->
                IO.puts("The insert")
                TxHandler.check_return!()
                :ets.insert(:dmsg, {hash, [hash, type, key, from, nonce, args, size]})
                :ets.insert(:msg, {hash, msg_sig})
                IO.puts("The result")
                %{"height" => height}
            end

          false ->
            ["error", "Deferred transaction already exists"]
        end
    end
  end

  def handle_request("last_round", _params, _state) do
    db_ref = :persistent_term.get(:main_conn)

    Round.last()
  end

  def handle_request("get_round", id, _state) do
    db_ref = :persistent_term.get(:main_conn)

    round =
      Round.fetch(id)
      |> Round.list_to_map()

    case Sqlite.fetch("get_jackpot") do
      nil ->
        round

      [winner, amount] ->
        Map.put(round, :jackpot, {winner, amount})
    end
  end

  def handle_request(_method, _data, _state), do: ["error", "Not found"]

  @impl Network
  def handle_message(_event, _data, _state), do: :ok
end
