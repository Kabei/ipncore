defmodule Ipncore.Endpoint do
  use Plug.Builder

  # if Mix.env() != :prod do
  plug(Plug.Logger)
  # end

  # plug(Plug.SSL, rewrite_on: [:x_forwarded_proto, :x_forwarded_host, :x_forwarded_port])
  plug(Plug.Parsers,
    parsers: [
      :urlencoded,
      # Increase to 4MB max upload
      {:multipart, length: 4_000_000},
      :json
    ],
    json_decoder: Jason
  )

  plug(Plug.Cors)

  plug(Ipncore.Explorer.Router)
end
