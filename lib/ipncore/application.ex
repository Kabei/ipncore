defmodule Ipncore.Application do
  @moduledoc false
  use Application
  require Logger

  # alias Ippan.Address

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]
  @default_node :nonode@nohost

  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  @impl true
  def start(_type, _args) do
    Logger.debug("Starting application")
    p2p_opts = Application.get_env(@otp_app, :p2p)
    data_dir = Application.get_env(@otp_app, :data_dir)
    http_opts = Application.get_env(@otp_app, :http)
    role = Application.get_env(@otp_app, :role)
    redis_url = Application.get_env(@otp_app, :redis)
    node_name = node()

    pubsub_verifiers_opts =
      if not is_nil(redis_url) and node_name != @default_node do
        [
          adapter: Phoenix.PubSub.Redis,
          url: redis_url,
          node_name: node_name,
          name: :verifiers
        ]
      else
        [name: :verifiers]
      end

    # create data folder
    File.mkdir(data_dir)

    # load falcon keys
    Ippan.P2P.Server.load_kem()
    Ippan.P2P.Server.load_key()

    #   # init chain
    #   :ok = Chain.start()

    # services
    children =
      case role do
        "miner" ->
          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            {WalletStore, Path.join(data_dir, "wallet/wallet.db")},
            {EnvStore, Path.join(data_dir, "env/env.db")},
            {ValidatorStore, Path.join(data_dir, "validator/validator.db")},
            {TokenStore, Path.join(data_dir, "token/token.db")},
            {BalanceStore, Path.join(data_dir, "txs/balance.db")},
            {RefundStore, Path.join(data_dir, "txs/refund.db")},
            {DomainStore, Path.join(data_dir, "domain/domain.db")},
            {DnsStore, Path.join(data_dir, "dns/dns.db")},
            {BlockStore, Path.join(data_dir, "chain/block.db")},
            {RoundStore, Path.join(data_dir, "chain/round.db")},
            {ThousandIsland, p2p_opts},
            Supervisor.child_spec({Phoenix.PubSub, pubsub_verifiers_opts}, id: :verifiers),
            Supervisor.child_spec({Phoenix.PubSub, name: :network}, id: :network),
            Supervisor.child_spec({Phoenix.PubSub, name: :miner}, id: :miner),
            {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :key_dir)},
            {BlockTimer, []},
            {EventChannel, %{server: :miner}}
          ]

        "verifier" ->
          miner_node =
            case System.get_env("MINER") || Application.get_env(@otp_app, :miner) do
              nil -> raise RuntimeError, "Set up a miner"
              x -> String.to_atom(x)
            end

          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            Supervisor.child_spec({Phoenix.PubSub, pubsub_verifiers_opts}, id: :verifiers),
            Supervisor.child_spec({Phoenix.PubSub, name: :network}, id: :network),
            Supervisor.child_spec({Phoenix.PubSub, name: :miner}, id: :miner),
            {EventChannel, %{server: :verifiers}},
            {NodeMonitor, [miner_node]},
            {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts}
          ]
      end

    case Supervisor.start_link(children, @opts) do
      {:ok, _pid} = result ->
        if role == "miner" do
          Platform.start()
          Logger.info("Running IPNcore P2P with port #{p2p_opts[:port]}")
        end

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
