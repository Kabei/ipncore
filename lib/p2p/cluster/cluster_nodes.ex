defmodule Ippan.ClusterNodes do
  require Ippan.Round
  alias Ippan.Round
  alias Ippan.{Node, Network, TxHandler, Wallet}
  require Ippan.{Node, TxHandler, Round}
  require BalanceStore
  require Sqlite
  require Logger

  @app Mix.Project.config()[:app]

  use Network,
    app: @app,
    name: :cluster,
    table: :cnw,
    server: Ippan.ClusterNode.Server,
    pubsub: :pubsub,
    topic: "cluster",
    opts: Application.compile_env(@app, :p2p_client),
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
    default_port = Application.get_env(@app, :cluster)[:port]
    :persistent_term.put(:msg_counter, :counters.new(1, []))

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
    hash_key = {from, nonce}

    case :ets.insert_new(:hash, {hash_key, nil}) do
      true ->
        dets = DetsPlux.get(:nonce)
        cache = DetsPlux.tx(dets, :cache_nonce)

        IO.puts("The nonce")

        case Wallet.update_nonce(dets, cache, from, nonce) do
          :error ->
            :ets.delete(:hash, hash_key)
            ["error", "Invalid nonce"]

          _ ->
            IO.puts("The check return")

            case TxHandler.check_return() do
              false ->
                :ets.delete(:hash, hash_key)
                raise IppanError, "Insufficient balance"

              _ ->
                IO.puts("The insert")
                cref = :persistent_term.get(:msg_counter)
                :counters.add(cref, 1, 1)
                ix = :counters.get(cref, 1)

                :ets.insert(:dmsg, {ix, [hash, type, from, nonce, args, size]})
                :ets.insert(:msg, {ix, msg_sig})
                IO.puts("The result")
                %{"height" => :persistent_term.get(:height, 0)}
            end
        end

      false ->
        :ets.delete(:hash, hash_key)
        ["error", "Already exists"]
    end
  end

  def handle_request(
        "new_msg",
        [true, [hash, type, key, from, nonce | rest], return],
        _state
      ) do
    hash_key = {from, nonce}

    case :ets.insert_new(:hash, {hash_key, nil}) do
      true ->
        height = :persistent_term.get(:height, 0)
        msg_key = {type, key}

        IO.puts("The same hash")

        case :ets.insert_new(:dhash, {msg_key, hash, height}) do
          true ->
            [args, msg_sig, size] = rest

            dets = DetsPlux.get(:nonce)
            cache = DetsPlux.tx(:nonce, :cache_nonce)

            IO.puts("The nonce")

            case Wallet.update_nonce(dets, cache, from, nonce) do
              :error ->
                :ets.delete(:hash, hash_key)
                :ets.delete(:dhash, msg_key)
                ["error", "Invalid nonce"]

              _ ->
                case TxHandler.check_return() do
                  false ->
                    :ets.delete(:hash, hash_key)
                    :ets.delete(:dhash, msg_key)
                    raise IppanError, "Insufficient balance"

                  _ ->
                    IO.puts("The insert")
                    cref = :persistent_term.get(:msg_counter)
                    :counters.add(cref, 1, 1)
                    ix = :counters.get(cref, 1)

                    :ets.insert(:dmsg, {ix, [hash, type, key, from, nonce, args, size]})
                    :ets.insert(:msg, {ix, msg_sig})
                    IO.puts("The result")
                    %{"height" => height}
                end
            end

          false ->
            :ets.delete(:hash, hash_key)
            ["error", "Deferred transaction already exists"]
        end

      false ->
        :ets.delete(:hash, hash_key)
        ["error", "Already exists"]
    end
  end

  def handle_request("last_round", _params, _state) do
    db_ref = :persistent_term.get(:main_conn)

    Round.last()
  end

  def handle_request("get_round", id, _state) do
    db_ref = :persistent_term.get(:main_conn)
    round = Round.get(id)

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
    limit = Map.get(params, "limit", 50) |> min(100) |> trunc()
    offset = Map.get(params, "offset", 0)

    case Sqlite.fetch_all("get_rounds", [round_id, limit, offset]) do
      [] ->
        []

      data ->
        Enum.map(data, fn x ->
          round = Round.list_to_map(x)

          case Sqlite.fetch("get_jackpot") do
            nil ->
              Map.put(round, "jackpot", {nil, 0})

            [winner, amount] ->
              Map.put(round, "jackpot", {winner, amount})
          end
        end)
    end
  end

  def handle_request(_method, _data, _state), do: ["error", "Not found"]

  @impl Network
  def handle_message(_event, _data, _state), do: :ok
end
