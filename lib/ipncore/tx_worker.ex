defmodule TxWorker do
  use GenServer
  require Logger
  @module __MODULE__

  def child_spec(args) do
    %{
      id: {@module, args.id},
      start: {@module, :start_link, [args]}
    }
  end

  def start_link(args) do
    :gen_server.start_link(@module, args, hibernate_after: 20_000)
  end

  @impl true
  def init(args) do
    Process.flag(:trap_exit, true)
    :persistent_term.put({@module, args.id}, self())
    {:ok, args}
  end

  @partitions (System.schedulers_online() - 1) |> max(1)
  @doc """
  Get all pids
  """
  def all do
    Enum.map(0..@partitions, &{&1, get(&1)})
    |> Enum.into(%{})
  end

  @doc """
  Get a pid from number of partition
  """
  def get(num) do
    :persistent_term.get({@module, num}, nil)
  end

  @impl true
  def handle_cast(
        {
          :run,
          {hash, _type_id, from, nonce, args, size, _signature},
          %{fun: fun, modx: module}
        },
        state = %{cref: cref, validator: validator, block: block_id, round: round_id, refs: refs}
      ) do
    source = %{
      hash: hash,
      refs: refs,
      from: from,
      round: round_id,
      block: block_id,
      validator: validator,
      nonce: nonce,
      size: size
    }

    try do
      case :erlang.apply(module, fun, [source | args]) do
        :error ->
          :counters.add(cref, 2, 1)

        {:error, _} ->
          :counters.add(cref, 2, 1)

        _ ->
          :counters.add(cref, 1, 1)
      end
    rescue
      _err ->
        :counters.add(cref, 2, 1)
    end

    {:noreply, state}
  end

  # def handle_cast(
  #       {:verify, source = %{args: args, type: %{fun: fun, mod: module}}},
  #       state = %{cref: cref}
  #     ) do
  #       tx = %{

  #       }

  #   TxHandler.valid?(tx)

  #   {:noreply, state}
  # end

  def handle_cast({:init, new_state}, state) do
    {:noreply, Map.merge(state, new_state)}
  end

  # @impl true
  # def handle_info({:EXIT, _pid, reason}, state = %{cref: cref}) do
  #   Logger.warning("#{inspect(reason)} in handle_info")
  #   :counters.add(cref, 1, 1)
  #   {:stop, reason, state}
  # end

  @impl true
  def terminate(_reason, state) do
    :persistent_term.erase({__MODULE__, state.id})
  end
end
