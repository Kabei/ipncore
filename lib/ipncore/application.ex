defmodule Ipncore.Application do
  @moduledoc false
  alias Phoenix.PubSub
  alias Ippan.{ClusterNodes, NetworkNodes}
  use Application
  # import Ippan.Utils, only: [to_atom: 1]

  @otp_app :ipncore
  @opts [strategy: :one_for_one, name: Ipncore.Supervisor]

  @impl true
  def start(_type, _args) do
    IO.puts("Starting application")
    start_node()
    make_folders()
    load_keys()

    store_dir = :persistent_term.get(:store_dir)
    wallet_path = Path.join(store_dir, "wallet.dets")
    balance_path = Path.join(store_dir, "balance.dets")
    stats_path = Path.join(store_dir, "stats.dets")

    # services
    children =
      [
        MemTables,
        {DetsPlux, [id: :wallet, file: wallet_path]},
        {DetsPlux, [id: :balance, file: balance_path]},
        {DetsPlux, [id: :stats, file: stats_path]},
        NetStore,
        MainStore,
        Supervisor.child_spec({PubSub, [name: :cluster]}, id: :cluster),
        Supervisor.child_spec({PubSub, [name: :network]}, id: :network),
        ClusterNodes,
        NetworkNodes,
        RoundManager,
        {Bandit, [plug: Ipncore.Endpoint, scheme: :http] ++ Application.get_env(@otp_app, :http)}
      ]

    Supervisor.start_link(children, @opts)
  end

  @impl true
  def stop(_state) do
    IO.puts("Stopping application")
  end

  defp start_node do
    vid = System.get_env("VID")
    name = System.get_env("NAME")

    cond do
      is_nil(vid) ->
        raise IppanStartUpError, "variable VID (ValidatorID) is missing"

      is_nil(name) ->
        raise IppanStartUpError, "variable NAME is missing"

      true ->
        :persistent_term.put(:name, name)
        :persistent_term.put(:vid, String.to_integer(vid))
    end
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
    save_dir = Path.join(data_dir, "store/save") |> String.to_charlist()
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
