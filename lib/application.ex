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
      # create data folder
      File.mkdir(Application.get_env(@otp_app, :data_dir, "data"))

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
        Ipncore.DNS.child_spec(),
        Supervisor.child_spec({Phoenix.PubSub, name: :events}, id: :events),
        dns_udp_server(),
        dns_tls_server(),
        http_server(),
        https_server()
      ]

      # start block builder
      # BlockBuilderWork.next()

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
    falcon_dir = Application.get_env(@otp_app, :falcon_dir)

    {falcon_pk, _falcon_sk} = falcon_dir |> falcon_read_file!()

    address = Address.hash(falcon_pk)
    Application.put_env(@otp_app, :address, address)
    Application.put_env(@otp_app, :address58, Address.to_text(address))
  end

  defp dns_udp_server do
    opts = Application.get_env(@otp_app, :dns)
    ip_address = Keyword.get(opts, :ip, {0, 0, 0, 0})
    port = Keyword.get(opts, :port, 53)
    {DNS.Server, [ip: ip_address, port: port]}
  end

  def dns_tls_server do
    opts = Application.get_env(@otp_app, :dns_tls)
    {ThousandIsland, opts}
  end

  defp dns_udp_ipv6_server do
    opts = Application.get_env(@otp_app, :dns6)
    ip_address = Keyword.get(opts, :ip, {0, 0, 0, 0, 0, 0, 0, 0})
    port = Keyword.get(opts, :port, 53)
    {DNS.Server, [ip: ip_address, port: port]}
  end

  defp http_server do
    opts = Application.get_env(@otp_app, :http)
    {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ opts}
  end

  defp https_server do
    opts = Application.get_env(@otp_app, :https)
    {Bandit, [plug: Ipncore.Endpoint, scheme: :https] ++ opts}
  end

  @file_version 0
  @type_alg "FAL"
  def falcon_read_file!(path) do
    <<file_version::8, type::bytes-size(3), rest::binary>> = File.read!(path)

    cond do
      file_version != @file_version ->
        throw(:bad_version)

      type != @type_alg ->
        throw(:bad_type)

      true ->
        cond do
          897 + 1281 == byte_size(rest) ->
            <<pk::bytes-size(897), sk::bytes-size(1281)>> = rest
            {pk, sk}

          1793 + 2305 == byte_size(rest) ->
            <<pk::bytes-size(1793), sk::bytes-size(2305)>> = rest
            {pk, sk}

          true ->
            throw(:bad_data)
        end
    end
  end

  defp falcon_write_file!(path, {pk, sk}) do
    data = <<@file_version>> <> @type_alg <> pk <> sk
    File.write!(path, data)
  end
end
