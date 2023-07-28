defmodule Ipncore.Application do
  @moduledoc false
  use Application
  require Logger
  import Ippan.Utils, only: [to_atom: 1, my_ip: 0]

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]
  @default_node :nonode@nohost
  @compile :native
  @compile {:hipe, [:verbose, :o3]}

  @impl true
  def start(_type, _args) do
    Logger.debug("Starting application")

    p2p_opts = Application.get_env(@otp_app, :P2P)
    data_dir = Application.get_env(@otp_app, :data_dir)
    http_opts = Application.get_env(@otp_app, :http)
    role = Application.get_env(@otp_app, :role)
    redis_url = System.get_env("REDIS")
    node_str = System.get_env("NODE")
    cookie = System.get_env("COOKIE")
    node_name = start_node(node_str, cookie)
    miner = System.get_env("MINER") |> to_atom()

    pubsub_verifiers_opts =
      unless is_nil(redis_url) and node_name != @default_node do
        [
          adapter: Phoenix.PubSub.Redis,
          url: redis_url,
          node_name: node_name,
          name: :verifiers
        ]
      else
        [name: :verifiers]
      end

    # create folders
    make_folders(role)

    # load falcon keys
    Ippan.P2P.Server.load_kem()
    Ippan.P2P.Server.load_key()

    Platform.start(role)

    # services
    children =
      case role do
        "miner" ->
          Sqlite3Tools.init()

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
            Supervisor.child_spec({Phoenix.PubSub, pubsub_verifiers_opts},
              id: :verifiers
            ),
            Supervisor.child_spec({Phoenix.PubSub, name: :network}, id: :network),
            # Supervisor.child_spec({Phoenix.PubSub, name: :miner}, id: :miner),
            {EventMinerChannel, []},
            {ThousandIsland, p2p_opts},
            {Ippan.P2P.ClientPool, Application.get_env(@otp_app, :key_dir)},
            {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts},
            {BlockTimer, []},
            {VoteCounter, []}
          ]

        "verifier" ->
          case miner do
            nil -> raise RuntimeError, "Set up a miner"
            _ -> :ok
          end

          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            {WalletStore, Path.join(data_dir, "wallet/wallet.db")},
            Supervisor.child_spec({Phoenix.PubSub, pubsub_verifiers_opts},
              id: :verifiers
            ),
            # Supervisor.child_spec({Phoenix.PubSub, name: :network}, id: :network),
            # Supervisor.child_spec({Phoenix.PubSub, name: :miner}, id: :miner),
            {NodeMonitor, miner},
            {BlockVerifierChannel, []},
            {EventVerifierChannel, []},
            {WalletVerifierChannel, []},
            {RoundChannel, []},
            {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts}
          ]

        "test" ->
          [
            {MessageStore, Path.join(data_dir, "requests/messages.db")},
            {WalletStore, Path.join(data_dir, "wallet/wallet.db")}
          ]
      end

    case Supervisor.start_link(children, @opts) do
      {:ok, _pid} = result ->
        if role == "miner" do
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

  # create all folders by role
  defp make_folders(_role) do
    # catch routes
    data_dir = Application.get_env(@otp_app, :data_dir)
    block_dir = Path.join(data_dir, "blocks")
    block_decode_dir = Path.join(data_dir, "blocks/decoded")
    # set variable
    Application.put_env(@otp_app, :block_dir, block_dir)
    Application.put_env(@otp_app, :decode_dir, block_decode_dir)
    # make folders
    File.mkdir(data_dir)
    File.mkdir(block_dir)
    File.mkdir(block_decode_dir)
  end

  defp start_node(name, cookie) when is_nil(name) or is_nil(cookie) do
    raise IppanError, "Set NODE and COOKIE variables"
  end

  defp start_node(name, cookie) do
    name = String.to_atom(name)
    Node.start(name)
    Node.set_cookie(name, String.to_atom(cookie))
    Application.put_env(@otp_app, :hostname, my_ip())
    name
  end
end
