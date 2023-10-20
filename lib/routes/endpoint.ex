defmodule Ipncore.Endpoint do
  use Plug.Builder

  if Mix.env() == :dev do
    plug(Plug.Logger)
  end

  plug(Ipncore.Router)
end
