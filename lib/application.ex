defmodule Ipncore.Application do
  @moduledoc false
  use Application

  alias Ipncore.{
    Block,
    Balance,
    Chain,
    DNSS,
    Domain,
    Event,
    Migration,
    Mempool,
    Repo,
    RepoWorker,
    Validator,
    Token,
    Wallet
  }

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}

  @impl true
  def start(_type, _args) do
    try do
      ## Ipncore.ConfigProvider.read()

      # imp_client = Application.get_env(@otp_app, :imp_client)

      # {falcon_pk, _sk} =
      #    @otp_app |> Application.app_dir(imp_client[:falcon_file]) |> Falcon.read_file!()

      http_config = Application.get_env(@otp_app, :web)

      # address = Address.to_internal_address(falcon_pk)
      # Application.put_env(@otp_app, :address, address)
      # Application.put_env(@otp_app, :address58, Base58Check.encode(address))

      # deliver
      # post_path =
      #   Application.get_env(@otp_app, :post_path)
      #   |> Path.expand()

      # Application.put_env(@otp_app, :post_path, post_path)

      # opts_cubdb = Application.get_env(@otp_app, :cubdb)

      # IO.inspect("imp_client")
      # IO.inspect(imp_client)

      # create data folder
      File.mkdir(Application.get_env(@otp_app, :data_path, "data"))

      # run migration
      migration_start()

      # open local databases
      with {:ok, _} <- Chain.open(),
           {:ok, _} <- Event.open(Block.epoch(Chain.next_index())),
           :mempool <- Mempool.open(),
           [{:ok, _} | _] <- Wallet.open(),
           {:ok, _} <- Balance.open(),
           {:ok, _} <- Token.open(),
           {:ok, _} <- Validator.open(),
           {:ok, _} <- Domain.open() do
        :ok
      else
        err -> throw(err)
      end

      # init chain
      :ok = Chain.initialize()

      children = [
        {DNSS, Application.get_env(@otp_app, :dns_port, 53)},
        Repo,
        RepoWorker,
        # {Ipncore.IMP.Client, imp_client},
        http_server(http_config)
      ]

      BlockBuilderWork.next()

      Supervisor.start_link(children, @opts)
    rescue
      DBConnection.ConnectionError ->
        {:error, "Database connexion failed"}
    end
  end

  defp migration_start do
    {:ok, supervisor} = Supervisor.start_link([Repo], @opts)

    # migration
    Migration.start()

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
