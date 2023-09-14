defmodule Ippan.ClusterNode do
  require Logger
  alias Ippan.{LocalNode, Network}
  require SqliteStore

  use Network,
    app: :ipncore,
    name: :cluster,
    table: :cnw,
    pubsub: :cluster,
    topic: "cluster",
    opts: Application.compile_env(:ipncore, :p2p_client),
    conn_opts: [reconnect: true, retry: :infinity],
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
        [false, msg = [hash | _], msg_sig],
        _state
      ) do
    IO.inspect(hash)

    case :ets.member(:hash, hash) do
      true ->
        IO.inspect("Already Exists")
        ["error", "Already exists"]

      false ->
        IO.inspect("No exists")
        height = :persistent_term.get(:height, 0)
        :ets.insert(:hash, {hash, height})
        :ets.insert(:dmsg, List.to_tuple(msg))
        :ets.insert(:msg, List.to_tuple(msg_sig))

        %{"height" => height}
    end
  end

  def handle_request(
        "new_msg",
        [true, msg = [hash, type, key | _], msg_sig],
        _state
      ) do
    case :ets.member(:hash, hash) do
      true ->
        ["error", "Already exists"]

      false ->
        height = :persistent_term.get(:height, 0)
        msg_key = {type, key}

        case :ets.insert_new(:dhash, {{type, key}, hash}) do
          true ->
            IO.inspect(msg)
            :ets.insert(:hash, {hash, height})
            IO.inspect("1")
            :ets.insert(:dhash, {msg_key, hash})
            IO.inspect("2")
            :ets.insert(:dmsg, List.to_tuple(msg))
            IO.inspect("3")
            :ets.insert(:msg, msg_sig)
            IO.inspect("4")

            %{"height" => height}

          false ->
            ["error", "Deferred transaction already exists"]
        end
    end
  end

  def handle_request(_method, _data, _state), do: ["error", "Not found"]

  @impl Network
  def handle_message(_event, _data, _state), do: :ok

  # defp check_table_size do
  #   if :ets.info(:msg, :memory) >= @max_block_data_size do
  #     nil
  #   end
  # end

  # import Ippan.Block,
  #   only: [decode_file!: 1, encode_file!: 1, hash_file: 1]

  # defp generate_files(creator_id, block_id) do
  #   filename = "#{creator_id}.#{block_id}.#{@block_extension}"
  #   block_path = Path.join(Application.get_env(:ipncore, :block_dir), filename)
  #   decode_path = Path.join(Application.get_env(:ipncore, :decode_dir), filename)
  #   ets_msg = :ets.whereis(:msg)
  #   ets_dmsg = :ets.whereis(:dmsg)

  #   messages = :ets.tab2list(ets_msg)
  #   decode_messages = :ets.tab2list(ets_dmsg)

  #   last_key =
  #     List.last(messages)
  #     |> elem(1)

  #   last_dkey =
  #     List.last(decode_messages)
  #     |> elem(1)

  #   File.write(block_path, encode_file!(messages))

  #   File.write(decode_path, encode_file!(decode_messages))

  #   ets_delete_while(ets_msg, last_key, :ets.first(ets_msg))
  #   ets_delete_while(ets_dmsg, last_dkey, :ets.first(ets_dmsg))

  #   {:ok, file_info} = File.stat(block_path)

  #   %{
  #     count: length(messages),
  #     creator: creator_id,
  #     hash: hash_file(block_path),
  #     height: block_id,
  #     size: file_info.size
  #   }
  # end

  # defp ets_delete_while(_table, _target, :"$end_of_table"), do: :ok

  # defp ets_delete_while(table, target, key) do
  #   if key != target do
  #     next = :ets.next(table, key)
  #     :ets.delete(table, key)
  #     ets_delete_while(table, target, next)
  #   else
  #     :ets.delete(table, key)
  #   end
  # end
end
