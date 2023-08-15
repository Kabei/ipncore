defmodule Ippan.P2P.PeerManager do
  use GenServer
  alias Ippan.Validator
  alias Ippan.P2P.{PeerSupervisor, PeerClient}
  alias Phoenix.PubSub
  require Logger
  require Global

  @port Application.compile_env(:ipncore, :port, 5815)
  @pubsub_server :cluster

  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  @impl true
  def init(key_path) do
    {:ok, validators} = ValidatorStore.all()
    myid = Global.validator_id()

    {:ok, spid} = PeerSupervisor.start_link([])

    peers =
      Enum.map(validators, fn x ->
        validator = Validator.to_map(x)
        validator_id = validator.id
        hostname = validator.hostname

        if myid != validator_id do
          {:ok, pid} =
            PeerSupervisor.start_child(%{
              hostname: hostname,
              port: @port,
              vid: validator_id,
              key_path: key_path
            })

          {validator_id, %{pid: pid, hostname: hostname}}
        end
      end)
      |> Enum.filter(fn
        nil -> false
        _ -> true
      end)
      |> Map.new()

    subscribe()

    {:ok, %{peers: peers, key_path: key_path, supervisor: spid}}
  end

  @impl true
  def handle_info(
        {"new", validator},
        %{
          peers: peers,
          key_path: key_path
        } = state
      ) do
    IO.puts("validator.new")
    IO.inspect(validator)

    validator_id = validator.id

    if Global.validator_id() != validator_id and not Map.has_key?(peers, validator_id) do
      hostname = validator.hostname
      {:ok, pid} = PeerClient.start_link({hostname, @port, validator_id, key_path})

      new_state = Map.put(peers, validator_id, %{pid: pid, hostname: hostname})
      {:noreply, new_state}
    else
      {:noreply, state, :hibernate}
    end
  end

  def handle_info(
        {"update", validator_id, %{hostname: new_hostname}},
        %{peers: peers, key_path: key_path} = state
      ) do
    if Global.validator_id() != validator_id do
      %{pid: old_pid} = Map.get(peers, validator_id)
      GenServer.stop(old_pid, :normal)
      {:ok, pid} = PeerClient.start_link({new_hostname, @port, validator_id, key_path})
      {:noreply, Map.put(peers, validator_id, %{pid: pid, hostname: new_hostname}), :hibernate}
    else
      {:noreply, state, :hibernate}
    end
  end

  def handle_info({"delete", validator_id}, %{peers: peers} = state) do
    if Global.validator_id() != validator_id do
      %{pid: pid} = Map.get(peers, validator_id)
      GenServer.stop(pid, :normal)
      Logger.info("Validator's licence deleted")
      {:noreply, Map.delete(peers, validator_id), :hibernate}
    else
      {:noreply, state, :hibernate}
    end
  end

  @impl true
  def terminate(reason, state) do
    unsubscribe()
    unlink(state, reason)
  end

  defp unlink(%{supervisor: spid}, reason) do
    DynamicSupervisor.stop(spid, reason)
  end

  defp subscribe do
    PubSub.subscribe(@pubsub_server, "validator")
  end

  defp unsubscribe do
    PubSub.unsubscribe(@pubsub_server, "validator")
  end
end
