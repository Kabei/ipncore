defmodule Ipncore.Application do
  @moduledoc false
  alias Phoenix.PubSub
  alias Ippan.{ClusterNodes, NetworkNodes, DetsSup}
  use Application

  @app Mix.Project.config()[:app]
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  @impl true
  def start(_type, _args) do
    IO.puts("Starting application")
    start_node()
    make_folders()
    load_keys()

    # services
    children =
      [
        MemTables,
        DetsSup,
        NetStore,
        MainStore,
        {PubSub, [name: :pubsub]},
        BlockTimer,
        ClusterNodes,
        NetworkNodes,
        RoundManager,
        {Bandit, Application.get_env(@app, :http)}
      ]

    Supervisor.start_link(children, @opts)
  end

  @impl true
  def stop(_state) do
    IO.puts("Stopping application")
    BlockTimer.save()
  end

  defp start_node do
    vid =
      System.get_env("VID") || raise IppanStartUpError, "variable VID (ValidatorID) is missing"

    name = System.get_env("NAME") || raise IppanStartUpError, "variable NAME is missing"

    :persistent_term.put(:name, name)
    :persistent_term.put(:vid, String.to_integer(vid))
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
    block_dir = Path.join(data_dir, "blocks") |> String.to_charlist()
    decode_dir = Path.join(data_dir, "blocks/decoded") |> String.to_charlist()
    store_dir = Path.join(data_dir, "store") |> String.to_charlist()
    save_dir = Path.join(data_dir, "save") |> String.to_charlist()
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
