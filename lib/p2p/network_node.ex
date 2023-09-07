defmodule Ippan.NetworkNode do
  alias Ippan.EventHandler
  alias Ippan.Validator
  alias Ippan.Network
  require SqliteStore

  @max_block_data_size Application.compile_env(:ipncore, :max_block_data_size)

  use Network,
    app: :ipncore,
    name: :network,
    table: :nw,
    pubsub: :network,
    topic: "network",
    opts: Application.compile_env(:ipncore, :p2p_client),
    sup: Ippan.NetworkSup

  @impl Network
  def fetch(id) do
    SqliteStore.lookup_map(
      :valdiator,
      :persistent_term.get(:asset_conn),
      :persistent_term.get(:asset_stmt),
      "get_validator",
      id,
      Validator
    )
  end

  @impl Network
  def handle_request("get_msg_rounds", data, _state) do
    conn = :persistent_term.get(:net_conn)
    stmts = :persistent_term.get(:net_stmt)
    limit = Map.get(data, "limit", 50) |> min(100) |> trunc()
    offset = Map.get(data, "offset", 0)
    SqliteStore.fetch_all(conn, stmts, "get_msg_rounds", limit, offset)
  end

  def handle_request(
        "new_msg",
        [[hash, type, timestamp, account_id, validator_id, size, args], msg_sig],
        _state
      ) do
    unless :ets.member(:msg, hash) do
      conn = :persistent_term.get(:asset_conn)
      stmts = :persistent_term.get(:asset_stmt)
      dets = :persistent_term.get(:dets_balance)
      round = :persistent_term.get(:round, 0)

      result =
        EventHandler.handle!(
          conn,
          stmts,
          dets,
          hash,
          type,
          timestamp,
          account_id,
          validator_id,
          size,
          args,
          round
        )

      if result != :error do
        :ets.insert(:msg, {hash, msg_sig})

        if :ets.info(:msg, :memory) > @max_block_data_size do
          # build block
        end
      end

      result
    else
      %{"error" => "already exists"}
    end
  end

  def handle_request(_method, _data, _state), do: "not found"

  @impl Network
  def handle_message("msg_round", data, %{id: from}) do
    # BlockTimer.new_round(data, from)

    # if from == BlockTimer.who_is_on_duty(data.id) do
    #   broadcast_except(%{"event" => "msg_round", "data" => data}, [from])
    # end
    send(RoundManager, {"msg_round", from, data})

    :ok
  end

  def handle_message(_event, _from, _data), do: :ok
end
