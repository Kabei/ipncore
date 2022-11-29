defmodule Ipncore.Application do
  @moduledoc false
  use Application

  alias Ipncore.{
    Address,
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

      # deliver
      # post_path =
      #   Application.get_env(@otp_app, :post_path)
      #   |> Path.expand()

      # Application.put_env(@otp_app, :post_path, post_path)

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
        Repo,
        RepoWorker,
        dns_server(),
        # imp_client(),
        http_server(),
        https_server()
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

  defp imp_client do
    opts = Application.get_env(@otp_app, :imp_client)
    cert_dir = opts[:cert_dir] || :code.priv_dir(@otp_app)

    {falcon_pk, _falcon_sk} = Path.join(cert_dir, "falcon.keys") |> Falcon.read_file!()

    address = Address.hash(falcon_pk)
    Application.put_env(@otp_app, :address, address)
    Application.put_env(@otp_app, :address58, Address.to_text(address))

    {Ipncore.IMP.Client, opts}
  end

  defp dns_server do
    opts = Application.get_env(@otp_app, :dns)
    ip_address = Keyword.get(opts, :ip, {0, 0, 0, 0})
    port = Keyword.get(opts, :port, 53)
    {DNSS, [ip_address, port]}
  end

  defp http_server do
    opts = Application.get_env(@otp_app, :http)

    {Plug.Cowboy,
     scheme: :http,
     plug: Ipncore.Endpoint,
     options: [
       net: opts[:net] || :inet,
       port: opts[:port] || 80,
       protocol_options: [
         idle_timeout: 60_000,
         max_keepalive: 5_000_000
       ],
       transport_options: [
         num_acceptors: opts[:acceptors] || 100,
         max_connections: opts[:max_conn] || 16_384
       ]
     ]}
  end

  defp https_server do
    opts = Application.get_env(@otp_app, :https)
    cert_dir = opts[:cert_dir] || :code.priv_dir(@otp_app)

    {Plug.Cowboy,
     scheme: :https,
     plug: Ipncore.Endpoint,
     options: [
       net: opts[:net] || :inet,
       compress: true,
       cipher_suite: :strong,
       otp_app: @otp_app,
       port: opts[:port] || 443,
       keyfile: Path.join(cert_dir, "key.pem"),
       certfile: Path.join(cert_dir, "cert.pem"),
       cacertfile: Path.join(cert_dir, "cacert.pem"),
       protocol_options: [
         idle_timeout: 60_000,
         max_keepalive: 5_000_000
       ],
       transport_options: [
         num_acceptors: opts[:acceptors] || 100,
         max_connections: opts[:max_conn] || 16_384
       ]
     ]}
  end
end
