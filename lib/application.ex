defmodule Ipncore.Application do
  @moduledoc false
  use Application
  require Logger

  alias Ipncore.{
    Address,
    Block,
    Balance,
    Chain,
    DNS,
    Domain,
    DnsRecord,
    Event,
    Migration,
    Repo,
    RepoWorker,
    Validator,
    Token,
    Tx,
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

      # load node config
      node_config()

      # run migration
      migration_start()

      # open local databases
      with {:ok, _pid} <- Chain.open(),
           {:ok, _pid} <- Event.open(Block.epoch(Chain.next_index())),
           :mempool <- Mempool.open(),
           [{:ok, _pid} | _rest] <- Wallet.open(),
           {:ok, _pid} <- Balance.open(),
           {:ok, _pid} <- Token.open(),
           {:ok, _pid} <- Validator.open(),
           {:ok, _pid} <- Tx.open(),
           {:ok, _pid} <- Domain.open(),
           {:ok, _pid} <- DnsRecord.open() do
        Platform.start()
      else
        err -> throw(err)
      end

      # init chain
      :ok = Chain.start()

      # services
      children = [
        Repo,
        RepoWorker,
        dns_udp_server(),
        dns_tls_server(),
        # dns_udp_ipv6_server(),
        # imp_client(),
        http_server(),
        https_server()
      ]

      # start block builder
      BlockBuilderWork.next()

      Supervisor.start_link(children, @opts)
    rescue
      DBConnection.ConnectionError ->
        {:error, "Database connexion failed"}
    end
  end

  @impl true
  def stop(_state) do
    Logger.info("Stopping application")
    Chain.close()
    Event.close()
    Wallet.close()
    Balance.close()
    Token.close()
    Validator.close()
    Tx.close()
    Domain.close()
    DnsRecord.close()
  end

  defp migration_start do
    {:ok, supervisor} = Supervisor.start_link([Repo], @opts)

    # migration
    Migration.start()

    :ok = Supervisor.stop(supervisor, :normal)
  end

  defp node_config do
    opts = Application.get_env(@otp_app, :https)
    cert_dir = opts[:cert_dir] || :code.priv_dir(@otp_app)

    {falcon_pk, _falcon_sk} = Path.join(cert_dir, "falcon.keys") |> Falcon.read_file!()

    address = Address.hash(falcon_pk)
    Application.put_env(@otp_app, :address, address)
    Application.put_env(@otp_app, :address58, Address.to_text(address))
  end

  # defp imp_client do
  #   opts = Application.get_env(@otp_app, :imp_client)
  #   {Ipncore.IMP.Client, opts}
  # end

  defp dns_udp_server do
    opts = Application.get_env(@otp_app, :dns)
    ip_address = Keyword.get(opts, :ip, {0, 0, 0, 0})
    port = Keyword.get(opts, :port, 53)
    {DNS.Server, [ip: ip_address, port: port]}
  end

  def dns_tls_server do
    tls_opts = Application.get_env(@otp_app, :dns_tls) || throw("DNS-over-TLS options not found")

    {
      ThousandIsland,
      [handler_module: Ipncore.DNS.TlsServer, transport_module: ThousandIsland.Transports.SSL] ++
        tls_opts
    }
  end

  defp dns_udp_ipv6_server do
    opts = Application.get_env(@otp_app, :dns6)
    ip_address = Keyword.get(opts, :ip, {0, 0, 0, 0, 0, 0, 0, 0})
    port = Keyword.get(opts, :port, 53)
    {DNS.Server, [ip: ip_address, port: port]}
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
