defmodule Ipncore.Application do
  @moduledoc false
  alias Phoenix.PubSub
  alias Ippan.{ClusterNode, NetworkNode}
  use Application
  # import Ippan.Utils, only: [to_atom: 1]

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  @impl true
  def start(_type, _args) do
    IO.puts("Starting application")
    # start erlang node
    start_node()

    # create folders
    make_folders()

    # load falcon keys
    load_keys()

    # services
    children =
      [
        MemTables,
        MainStore,
        Supervisor.child_spec({PubSub, [name: :cluster]}, id: :cluster),
        Supervisor.child_spec({PubSub, [name: :network]}, id: :network),
        ClusterNode,
        NetworkNode,
        {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ Application.get_env(@otp_app, :http)}
      ]

    Supervisor.start_link(children, @opts)
  end

  @impl true
  def stop(_state) do
    IO.puts("Stopping application")
  end

  defp start_node do
    :persistent_term.put(:node, System.get_env("NODE"))
    :persistent_term.put(:vid, String.to_integer(System.get_env("VID", "0")))
    # name = System.get_env("NODE") |> to_atom()
    # cookie = System.get_env("COOKIE") |> to_atom()

    # {:ok, _} = Node.start(name)
    # Node.set_cookie(name, cookie)
  end

  defp load_keys do
    seed_kem = System.get_env("CLUSTER_KEY") |> Fast64.decode64()
    seed = System.get_env("SECRET_KEY") |> Fast64.decode64()

    {:ok, net_pubkey, net_privkey} = NtruKem.gen_key_pair_from_seed(seed_kem)
    {:ok, {pubkey, privkey}} = Cafezinho.Impl.keypair_from_seed(seed)

    :persistent_term.put(:pubkey, pubkey)
    :persistent_term.put(:privkey, privkey)
    :persistent_term.put(:net_pubkey, net_pubkey)
    :persistent_term.put(:net_privkey, net_privkey)
  end

  # create all folders
  defp make_folders do
    # catch routes
    data_dir = System.get_env("data_dir", "data")
    block_dir = Path.join(data_dir, "blocks")
    decode_dir = Path.join(data_dir, "blocks/decoded")
    store_dir = Path.join(data_dir, "store")
    save_dir = Path.join(data_dir, "store/save")
    # set variables
    :persistent_term.put(:data_dir, data_dir)
    :persistent_term.put(:block_dir, block_dir)
    :persistent_term.put(:decode_dir, decode_dir)
    :persistent_term.put(:store_dir, store_dir)
    :persistent_term.put(:save_dir, save_dir)
    # make folders
    File.mkdir(data_dir)
    File.mkdir(store_dir)
    File.mkdir(block_dir)
    File.mkdir(decode_dir)
    File.mkdir(save_dir)
  end
end
