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

    pubsub2_opts =
      if redis_url do
        [
          adapter: Phoenix.PubSub.Redis,
          url: redis_url,
          node_name: node,
          name: :pubsub2
        ]
      else
        [name: :pubsub2]
      end

    # create data folder
    File.mkdir(data_dir)

    # load falcon keys
    Ippan.P2P.Server.load_kem()

    #   # init chain
    #   :ok = Chain.start()

    # HashList.start(:l1)
    # HashList.start(:l2)

    # services
    children = [
      # {DetsPlus,
      #  [
      #    name: :account,
      #    file: String.to_charlist(Path.join(data_dir, "account.db")),
      #    keypos: :id,
      #    auto_save: :infinity
      #  ]},
      {MessageStore, [path: Path.join(data_dir, "requests/messages.db")]},
      # {GlobalRequestStore, path: Path.join(data_dir, "requests/grq.db")},
      {WalletStore, [path: Path.join(data_dir, "wallet/wallet.db")]},
      # {AccountStore, Path.join(data_dir, "account/account.db")},
      {EnvStore, [path: Path.join(data_dir, "env/env.db")]},
      {ValidatorStore, [path: Path.join(data_dir, "validator/validator.db")]},
      {TokenStore, [path: Path.join(data_dir, "token/token.db")]},
      {BalanceStore, [path: Path.join(data_dir, "txs/balance.db")]},
      {RefundStore, [path: Path.join(data_dir, "txs/refund.db")]},
      {DomainStore, [path: Path.join(data_dir, "domain/domain.db")]},
      {DnsStore, [path: Path.join(data_dir, "dns/dns.db")]},
      {BlockStore, [path: Path.join(data_dir, "chain/block.db")]},
      {RoundStore, [path: Path.join(data_dir, "chain/round.db")]},
      # pubsub
      Supervisor.child_spec({Phoenix.PubSub, pubsub2_opts}, id: :pubsub2),
      Supervisor.child_spec({Phoenix.PubSub, name: :pubsub}, id: :pubsub),
      # p2p
      {ThousandIsland, p2p_opts},
      {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :falcon_dir)},
      # http
      {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts}
    ]

    case Supervisor.start_link(children, @opts) do
      {:ok, _pid} = result ->
        Platform.start()
        BlockBuilderWork.run()
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
