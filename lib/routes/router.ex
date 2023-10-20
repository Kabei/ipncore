defmodule Ipncore.Router do
  use Plug.Router

  plug(:match)
  plug(:dispatch)

  forward("/v1/dl", to: Ipncore.FileRoutes)

  match _ do
    send_resp(conn, 404, "")
  end
end
