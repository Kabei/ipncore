defmodule Ipncore.Explorer.Router do
  use Plug.Router

  import Ipncore.WebTools, only: [json: 2, send_result: 2]
  import Ipnutils.Filters
  import Ipncore.PostDeliver, only: [serve_video: 3]

  alias Ipncore.{
    Address,
    Block,
    # Channel,
    Event,
    Tx,
    Txo,
    Token,
    Balance,
    Chain,
    Validator,
    Domain,
    DnsRecord
  }

  if Mix.env() == :dev do
    use Plug.Debugger
  end

  use Plug.ErrorHandler

  plug(:match)
  plug(:dispatch)

  get "/blockchain/blocks" do
    params = conn.params

    resp = Block.all(params)

    send_result(conn, resp)
  end

  get "/blockchain/events" do
    params = conn.params

    resp = Event.all(params)

    send_result(conn, resp)
  end

  get "/blockchain/txs" do
    params = conn.params
    resp = Tx.all(params)
    send_result(conn, resp)
  end

  get "/blockchain/txo" do
    params = conn.params
    resp = Txo.all(params)
    send_result(conn, resp)
  end

  # get "/txi" do
  #   params = conn.params
  #   resp = Txi.all(params)
  #   send_result(conn, resp)
  # end

  get "/blockchain/tokens" do
    params = conn.params
    resp = Token.all(params)
    send_result(conn, resp)
  end

  get "/blockchain/token/:token/:channel" do
    resp = Token.one(token, channel, conn.params)
    send_result(conn, resp)
  end

  get "/blockchain/validators" do
    params = conn.params
    resp = Validator.all(params)
    send_result(conn, resp)
  end

  get "/blockchain/validators/:hostname/:channel" do
    resp = Validator.one(hostname, channel, conn.params)
    send_result(conn, resp)
  end

  get "/blockchain/search" do
    params = conn.params

    case params do
      %{"q" => query} ->
        resp =
          cond do
            Regex.match?(Const.Regex.only_digits(), query) ->
              case Block.get(%{"height" => query}) do
                nil ->
                  nil

                b ->
                  %{"id" => b.height, "type" => "block"}
              end

            Regex.match?(Const.Regex.hash256(), query) ->
              hash = Base.decode16!(query, case: :mixed)

              case Block.get(%{"hash" => hash}) do
                nil ->
                  case Tx.one_by_hash(hash, params) do
                    nil ->
                      nil

                    tx ->
                      %{"id" => tx.index, "type" => "tx"}
                  end

                block ->
                  %{"id" => block.height, "type" => "block"}
              end

            Regex.match?(Const.Regex.address(), query) ->
              address = Address.from_text(query)

              case Balance.fetch_balance(address, Default.token(), Default.channel()) do
                nil ->
                  index = Event.decode_id(query)
                  Tx.one(index, params)

                x ->
                  %{"id" => x.address, "type" => "balance"}
              end

            Regex.match?(Const.Regex.base62(), query) ->
              index = Event.decode_id(query)

              case Tx.one(index, params) do
                nil ->
                  nil

                tx ->
                  %{"id" => tx.index, "type" => "tx"}
              end

            true ->
              nil
          end

        send_result(conn, resp)

      _ ->
        send_resp(conn, 400, "")
    end
  end

  get "/blockchain/balance/:address58/:token" do
    address = Address.from_text(address58)
    resp = Balance.fetch_balance(address, token, Default.channel())
    send_result(conn, resp)
  end

  get "/blockchain/balance/:address58" do
    address = Address.from_text(address58)
    resp = Balance.all_balance(address, conn.params)
    send_result(conn, resp)
  end

  get "/blockchain/activity/:address58" do
    resp = Balance.activity(address58, conn.params)

    send_result(conn, resp)
  end

  # get "/blockchain/channel/:channel_id" do
  #   resp =
  #     case Channel.get(channel_id) do
  #       nil ->
  #         send_error(conn, 404)

  #       channel ->
  #         json(conn, channel)
  #     end

  #   send_result(conn, resp)
  # end

  get "/blockchain/status" do
    # channel = Channel.get(channel_id)
    token_id = Default.token()
    token = Token.fetch!(token_id)
    last_block = Chain.last_block()
    coins = Tx.calc_amount_dec(token.supply, token.decimals)

    resp = %{
      coins: coins,
      height: last_block.height,
      last_hash: Event.encode_id(last_block.hash),
      owner: Platform.address58(),
      time: Chain.get_time(),
      token: token_id
      # ev_count: 
      # token_count:
      # domain_count:
    }

    send_result(conn, resp)
  end

  get "/blockchain/block/:hash16" when byte_size(hash16) == 64 do
    hash = Base.decode16!(hash16, case: :mixed)

    resp = Block.get(%{"hash" => hash})

    send_result(conn, resp)
  end

  get "/blockchain/block/height/:height" do
    resp = Block.get(%{"height" => height})

    send_result(conn, resp)
  end

  get "/blockchain/tx/:hash16" when byte_size(hash16) == 64 do
    hash = Base.decode16!(hash16, case: :mixed)

    resp = Tx.one(hash, conn.params)

    send_result(conn, resp)
  end

  get "/blockchain/event/:evid" do
    id = Event.decode_id(evid)
    resp = Event.one(id, filter_channel(conn.params, Default.channel()))

    send_result(conn, resp)
  end

  get "/blockchain/dns" do
    resp = DnsRecord.all(conn.params)
    send_result(conn, resp)
  end

  get "/blockchain/dns/:domain/:type" do
    channel = filter_channel(conn.params, Default.channel())
    resp = DnsRecord.one(domain, type, channel)
    send_result(conn, resp)
  end

  get "/blockchain/domains" do
    resp = Domain.all(conn.params)
    send_result(conn, resp)
  end

  get "/blockchain/domain/:name" do
    params = conn.params
    channel = filter_channel(params, Default.channel())
    resp = Domain.one(name, channel, params)
    send_result(conn, resp)
  end

  post "/blockchain/event" do
    # IO.inspect(conn)
    %{"_json" => body} = conn.params
    # IO.inspect(body)
    case body do
      [version, type_name, time, event_body, address, sig64] ->
        Event.check(version, type_name, time, event_body, address, sig64)

      [version, type_name, time, event_body, sig64] ->
        Event.check(version, type_name, time, event_body, sig64)
    end
    |> case do
      {:ok, event_id} ->
        # Ipncore.IMP.Client.publish("tx:" <> tx.index, params)
        json(conn, %{"hash" => Base.encode16(event_id, case: :lower)})

      {:error, err_message} ->
        send_resp(conn, 400, err_message)

      err_message ->
        # send_error(conn, err_message)
        send_resp(conn, 400, err_message)
    end
  end

  # serve posts
  get "/p/:id/:stream/:segment" do
    base = ["http://", Application.get_env(:ipncore, :central)] |> IO.iodata_to_binary()

    url =
      [base, "p", id, stream, segment]
      |> Path.join()

    headers = [
      {"cache-control", "private, max-age=21600"},
      {"content-type", "video/iso.segment"},
      {"address", Default.address58()}
    ]

    serve_video(conn, url, headers)
  end

  get "/p/:id/:manifest" do
    base = ["http://", Application.get_env(:ipncore, :central)] |> IO.iodata_to_binary()

    url =
      [base, "p", id, manifest]
      |> Path.join()

    headers = [
      {"cache-control", "private, max-age=21600"},
      {"content-type", "application/dash+xml"},
      {"address", Default.address58()}
    ]

    serve_video(conn, url, headers)
  end

  match _ do
    send_resp(conn, 404, "oops")
  end

  defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_resp(conn, conn.status, "Something went wrong")
  end
end
