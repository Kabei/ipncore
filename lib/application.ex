defmodule Ipncore.Application do
  @moduledoc false
  use Application
  alias Ipncore.{Chain, IIT, Migration, Repo}
  alias Ipnutils.Address
  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}

  @impl true
  def start(_type, _args) do
    try do
      # Ipncore.ConfigProvider.read()
      # central = Application.get_env(:ipncore, :central)
      imp_client = Application.get_env(@otp_app, :imp_client)

      {falcon_pk, _sk} =
        :ipncore
        |> Application.app_dir(imp_client[:falcon_file])
        |> Falcon.read_file!()

      address = Address.to_internal_address(falcon_pk)

      web = Application.get_env(@otp_app, :web)
      Application.put_env(@otp_app, :address, address)
      Application.put_env(@otp_app, :address58, Base58Check.encode(address))

      # deliver
      post_path =
        Application.get_env(@otp_app, :post_path)
        |> Path.expand()

      Application.put_env(@otp_app, :post_path, post_path)

      opts_cubdb = Application.get_env(@otp_app, :cubdb)

      migration_start()

      IO.inspect("imp_client")
      IO.inspect(imp_client)

      children =
        [
          Repo,
          {Ipncore.IMP.Client, imp_client},
          Chain,
          http_server(web)
        ] ++ Ipnutils.CubDB.children(opts_cubdb)

      Supervisor.start_link(children, @opts)
    rescue
      DBConnection.ConnectionError ->
        {:error, "Database connexion failed"}
    end
  end

  defp migration_start do
    {:ok, supervisor} = Supervisor.start_link([Repo, Chain], @opts)

    channel = Application.get_env(@otp_app, :channel)
    # migration
    Migration.start()

    iit = IIT.sync()
    Chain.put_iit(iit)
    Chain.initialize(channel)

    :ok = Supervisor.stop(supervisor, :normal)
  end

  defp http_server(opts) do
    {Plug.Cowboy,
     scheme: :http,
     plug: Ipncore.Endpoint,
     options: [
       port: opts[:http] || 80,
       protocol_options: [idle_timeout: 60_000, max_keepalive: 5_000_000]
     ]}
  end
end
