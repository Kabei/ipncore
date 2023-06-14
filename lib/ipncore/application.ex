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
    Logger.debug("Starting application")
    node = System.get_env("NODE", "n1")
    p2p_opts = Application.get_env(@otp_app, :p2p)
    data_dir = Application.get_env(@otp_app, :data_dir, "data")
    http_opts = Application.get_env(@otp_app, :http)
    redis_url = Application.get_env(@otp_app, :redis)

    role = System.get_env("ROLE", "verifier")

    pubsubi_opts =
      if redis_url do
        [
          adapter: Phoenix.PubSub.Redis,
          url: redis_url,
          node_name: node,
          name: :pubsubi
        ]
      else
        [name: :pubsubi]
      end

    # create data folder
    File.mkdir(data_dir)

    # load falcon keys
    Ippan.P2P.Server.load_kem()

    #   # init chain
    #   :ok = Chain.start()

    # services
    children =
      case role do
        "miner" ->
          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            {WalletStore, Path.join(data_dir, "wallet/wallet.db")},
            # WalletStore.child_spec(Path.join(data_dir, "wallet/wallet.db")),
            {EnvStore, Path.join(data_dir, "env/env.db")},
            {ValidatorStore, Path.join(data_dir, "validator/validator.db")},
            {TokenStore, Path.join(data_dir, "token/token.db")},
            {BalanceStore, Path.join(data_dir, "txs/balance.db")},
            # BalanceStore.child_spec(Path.join(data_dir, "txs/refund.db")),
            {RefundStore, Path.join(data_dir, "txs/refund.db")},
            {DomainStore, Path.join(data_dir, "domain/domain.db")},
            {DnsStore, Path.join(data_dir, "dns/dns.db")},
            {BlockStore, Path.join(data_dir, "chain/block.db")},
            {RoundStore, Path.join(data_dir, "chain/round.db")},
            {ThousandIsland, p2p_opts},
            Supervisor.child_spec({Phoenix.PubSub, pubsubi_opts}, id: :pubsubi),
            Supervisor.child_spec({Phoenix.PubSub, name: :pubsub}, id: :pubsub),
            {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :falcon_dir)},
            {BlockTimer, []}
          ]

        "verifier" ->
          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            Supervisor.child_spec({Phoenix.PubSub, pubsubi_opts}, id: :pubsubi),
            Supervisor.child_spec({Phoenix.PubSub, name: :pubsub}, id: :pubsub),
            {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts}
          ]
      end

    # children = [
    #   # # {AccountStore, Path.join(data_dir, "account/account.db")},
    #   {MessageStore, Path.join(data_dir, "requests/messages.db")},
    #   {WalletStore, Path.join(data_dir, "wallet/wallet.db")},
    #   # WalletStore.child_spec(Path.join(data_dir, "wallet/wallet.db")),
    #   {EnvStore, Path.join(data_dir, "env/env.db")},
    #   {ValidatorStore, Path.join(data_dir, "validator/validator.db")},
    #   {TokenStore, Path.join(data_dir, "token/token.db")},
    #   {BalanceStore, Path.join(data_dir, "txs/balance.db")},
    #   # BalanceStore.child_spec(Path.join(data_dir, "txs/refund.db")),
    #   {RefundStore, Path.join(data_dir, "txs/refund.db")},
    #   {DomainStore, Path.join(data_dir, "domain/domain.db")},
    #   {DnsStore, Path.join(data_dir, "dns/dns.db")},
    #   {BlockStore, Path.join(data_dir, "chain/block.db")},
    #   {RoundStore, Path.join(data_dir, "chain/round.db")},
    #   # # pubsub
    #   # Supervisor.child_spec({Phoenix.PubSub, pubsub2_opts}, id: :pubsubi),
    #   # Supervisor.child_spec({Phoenix.PubSub, name: :pubsub}, id: :pubsub),
    #   # p2p
    #   # {ThousandIsland, p2p_opts},
    #   # {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :falcon_dir)}
    #   # http
    #   {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts}
    # ]

    case Supervisor.start_link(children, @opts) do
      {:ok, _pid} = result ->
        if role == "miner" do
          Platform.start()
          # BlockBuilderWork.run()
        end

        Logger.info("Running IPNcore P2P with port #{p2p_opts[:port]}")
        result

      error ->
        error
    end
  end

  @impl true
  def stop(_state) do
    Logger.info("Stopping application")
  end
end
