defmodule Ipncore.Repo do
  use Ecto.Repo,
    otp_app: :ipncore,
    adapter: Ecto.Adapters.Postgres

  @module __MODULE__

  @spec table_exists?(schema :: String.t(), table_name :: String.t()) :: boolean()
  def table_exists?(schema, table_name) do
    qry =
      "SELECT true FROM information_schema.tables WHERE table_name = $2 AND table_schema = $1 LIMIT 1"

    res = Ecto.Adapters.SQL.query!(@module, qry, [schema, table_name])

    match?(%{rows: [[true]]}, res)
  end

  @spec schema_exists?(schema :: String.t()) :: boolean()
  def schema_exists?(schema) do
    qry = "SELECT true FROM information_schema.tables WHERE table_schema = $1 LIMIT 1"

    res = Ecto.Adapters.SQL.query!(@module, qry, [schema])

    match?(%{rows: [[true]]}, res)
  end
end
