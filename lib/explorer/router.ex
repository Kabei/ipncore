defmodule Ipncore.Router do
  use Plug.Router
  use Plug.ErrorHandler

  @dns_headers [
    {"content-type", "application/dns-message"},
    {"accept", "application/dns-message"}
  ]

  plug(:match)
  plug(:dispatch)

  forward("/blockchain", to: Ipncore.Route.Blockchain)

  post "/dns-query" do
    case get_req_header(conn, "accept") do
      ["application/dns-message"] ->
        {:ok, request_body, _} = read_body(conn)

        case request_body do
          "" ->
            conn
            |> merge_resp_headers(@dns_headers)
            |> send_resp(400, "")

          request_body ->
            response_body = Ipncore.DNS.handle(request_body, 0) || ""

            conn
            |> merge_resp_headers(@dns_headers)
            |> send_resp(200, response_body)
        end

      _ ->
        send_resp(conn, 400, "")
    end
  end

  match _ do
    send_resp(conn, 404, "oops")
  end

  defp handle_errors(conn, %{kind: _kind, reason: _reason, stack: _stack}) do
    send_resp(conn, conn.status, "Something went wrong")
  end
end
