defmodule Ipncore.Application do
  @moduledoc false
  use Application
  require Logger

  # alias Ippan.Address

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  # @compile :native
  # @compile {:hipe, [:verbose, :o3]}

  @impl true
  def start(_type, _args) do
    Logger.info("Starting application")
    # try do
    #   # create data folder
    server_opts = Application.get_env(@otp_app, :p2p)
    data_dir = Application.get_env(@otp_app, :data_dir, "data")
    File.mkdir(data_dir)

    # load falcon keys
    Ippan.P2P.Server.load_kem()
    #   # load node config
    #   node_config()

    #   # run migration
    #   migration_start()

    #   # open local databases
    # with {:ok, _pid} <- Chain.open(),
    #      {:ok, _pid} <- Event.open(Block.epoch(Chain.next_index())),
    #      :mempool <- Mempool.open(),
    #      [{:ok, _pid} | _rest] <- Wallet.open(),
    #      {:ok, _pid} <- Balance.open(),
    #      {:ok, _pid} <- Token.open(),
    #      {:ok, _pid} <- Validator.open(),
    #      {:ok, _pid} <- Tx.open(),
    #      {:ok, _pid} <- Domain.open(),
    #      {:ok, _pid} <- DnsRecord.open() do
    #   Platform.start()
    # else
    #   err -> throw(err)
    # end

    #   # init chain
    #   :ok = Chain.start()

    HashList.start(:l1)
    HashList.start(:l2)

    #   # services
    children = [
      # {DetsPlus,
      #  [
      #    name: :account,
      #    file: String.to_charlist(Path.join(data_dir, "account.db")),
      #    keypos: :id,
      #    auto_save: :infinity
      #  ]},
      RequestStore,
      {AccountStore, Path.join(data_dir, "account/account.db")},
      {EnvStore, Path.join(data_dir, "env/env.db")},
      {ValidatorStore, Path.join(data_dir, "validator/validator.db")},
      {TokenStore, Path.join(data_dir, "token/token.db")},
      {BalanceStore, Path.join(data_dir, "txs/balance.db")},
      {RefundStore, Path.join(data_dir, "txs/refund.db")},
      {DomainStore, Path.join(data_dir, "domain/domain.db")},
      {DnsStore, Path.join(data_dir, "dns/dns.db")},
      {BlockStore, Path.join(data_dir, "chain/block.db")},
      {RoundStore, Path.join(data_dir, "chain/round.db")},
      Supervisor.child_spec({Phoenix.PubSub, name: :pubsub}, id: :pubsub),
      {ThousandIsland, server_opts},
      {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :falcon_dir)}
    ]

    #   # start block builder
    #   BlockBuilderWork.next()

    Supervisor.start_link(children, @opts)
    # rescue
    #   DBConnection.ConnectionError ->
    #     {:error, "Database connexion failed"}
    # end
  end

  @impl true
  def stop(_state) do
    Logger.info("Stopping application")
  end
end
