defmodule Ipncore.Application do
  @moduledoc false
  alias Phoenix.PubSub
  use Application
  import Ippan.Utils, only: [my_ip: 0]

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  @impl true
  def start(_type, _args) do
    IO.puts("Starting application")

    p2p_opts = Application.get_env(@otp_app, :P2P)
    data_dir = Application.get_env(@otp_app, :data_dir)
    http_opts = Application.get_env(@otp_app, :http)
    node_str = System.get_env("NODE")
    cookie = System.get_env("COOKIE")
    start_node(node_str, cookie)

    # create folders
    make_folders()

    # load falcon keys
    Ippan.P2P.Server.load_kem()
    Ippan.P2P.Server.load_key()

    # Get sqlite tools
    # t = Task.async(fn -> Sqlite3Tools.init() end)
    # Task.await(t, :infinity)

    # services
    children =
      [
        {MessageStore, Path.join(data_dir, "store/priv/messages.db")},
        {WalletStore, Path.join(data_dir, "store/wallet.db")},
        {EnvStore, Path.join(data_dir, "store/env.db")},
        {ValidatorStore, Path.join(data_dir, "store/validator.db")},
        {TokenStore, Path.join(data_dir, "store/token.db")},
        {BalanceStore, Path.join(data_dir, "store/balance.db")},
        {RefundStore, Path.join(data_dir, "store/refund.db")},
        {DomainStore, Path.join(data_dir, "store/domain.db")},
        {DnsStore, Path.join(data_dir, "store/dns.db")},
        {BlockStore, Path.join(data_dir, "store/block.db")},
        {RoundStore, Path.join(data_dir, "store/round.db")},
        Supervisor.child_spec({PubSub, [name: :cluster]}, id: :cluster),
        Supervisor.child_spec({PubSub, name: :network}, id: :network),
        {BlockTimer, []},
        {EventMinerChannel, []},
        {ThousandIsland, p2p_opts},
        {Ippan.P2P.PeerManager, Application.get_env(@otp_app, :key_dir)},
        {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ http_opts},
        {VoteCounter, []}
      ]

    case Supervisor.start_link(children, @opts) do
      {:ok, _pid} = result ->
        IO.puts("Running IPNcore P2P with port #{p2p_opts[:port]}")
        result

      error ->
        error
    end
  end

  @impl true
  def stop(_state) do
    IO.puts("Stopping application")
  end

  # create all folders by role
  defp make_folders do
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
