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
    :persistent_term.put(:msg_counter, :counters.new(1, []))
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
            ["error", "Invalid nonce x1"]

          _ ->
            IO.puts("The check return")

            case TxHandler.check_return() do
              false ->
                :ets.delete(:hash, hash_key)
                Wallet.revert_nonce(cache, from)
                raise IppanError, "Insufficient balance x1"

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
            cache = DetsPlux.tx(dets, :cache_nonce)

            IO.puts("The nonce")

            case Wallet.update_nonce(dets, cache, from, nonce) do
              :error ->
                :ets.delete(:hash, hash_key)
                :ets.delete(:dhash, msg_key)
                ["error", "Invalid nonce x2"]

              _ ->
                case TxHandler.check_return() do
                  false ->
                    :ets.delete(:hash, hash_key)
                    :ets.delete(:dhash, msg_key)
                    Wallet.revert_nonce(cache, from)
                    raise IppanError, "Insufficient balance x2"

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
  def handle_message(event = "node.join", data, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:net_conn)

    if Node.insert(Node.to_list(data)) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.update", data = %{"data" => fields, "id" => id}, %{
        "id" => node_id
      }) do
    db_ref = :persistent_term.get(:net_conn)

    if Node.update(fields, id) == :done do
      broadcast_except(%{"event" => event, "data" => data}, [node_id])
    end
  end

  def handle_message(event = "node.leave", id, %{"id" => node_id}) do
    db_ref = :persistent_term.get(:net_conn)

    if Node.delete(id) == :done do
      broadcast_except(%{"event" => event, "data" => id}, [node_id])
    end
  end

  def handle_message(_event, _data, _state), do: :ok
end
