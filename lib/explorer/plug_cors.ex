defmodule Plug.Cors do
  import Plug.Conn

  def init(opts) do
    opts
  end

  def call(conn = %{method: "OPTIONS"}, _options) do
    conn
    |> merge_resp_headers([
      {"access-control-allow-headers",
       "Authorization, Content-Type, Accept, Origin, User-Agent, Accept-Encoding, Cache-Control, Keep-Alive"},
      {"access-control-allow-methods", "GET, POST, OPTIONS"},
      {"server", "IPNcore"}
    ])
  end

  def call(conn, _opts) do
    conn
    |> merge_resp_headers([
      {"access-control-allow-credentials", "true"},
      {"access-control-allow-origin", "*"},
      {"server", "IPNcore"}
    ])
  end
end