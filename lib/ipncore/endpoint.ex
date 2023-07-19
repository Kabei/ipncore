defmodule Ipncore.Endpoint do
  use Plug.Builder

  # @json Application.compile_env(:ipncore, :json)

  if Mix.env() == :dev do
    plug(Plug.Logger)
  end

  # plug(Plug.SSL, rewrite_on: [:x_forwarded_proto, :x_forwarded_host, :x_forwarded_port])
  plug(Plug.RewriteOn, [:x_forwarded_host, :x_forwarded_port, :x_forwarded_proto])

  # plug(Plug.Parsers,
  #   parsers: [
  #     # :urlencoded,
  #     # Increase to 4MB max upload
  #     # {:multipart, length: 4_000_000},
  #     {:json, json_decoder: @json}
  #   ],
  #   pass: ["*/*"]
  # )

  # plug(Plug.Cors)

  plug(Ipncore.Router)
end
