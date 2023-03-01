defmodule Ipncore.Router do
  use Plug.Router
  plug(:match)
  plug(:dispatch)
  forward("/blockchain", to: Ipncore.Route.Blockchain)
end
