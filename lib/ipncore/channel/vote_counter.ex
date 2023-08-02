defmodule VoteCounter do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, P2P}
  require Logger
  require Global

  @module __MODULE__
  @votes :votes
  @candidates :candidates
  @winners :winners
  @ets_opts [
    :set,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__, hibernate_after: 5_000)
  end

  defmacrop calc_minimum(validators) do
    quote do
      cond do
        unquote(validators) > 50 ->
          round(unquote(validators) / 3)

        unquote(validators) > 2 ->
          round(unquote(validators) / 2)

        true ->
          1
      end
    end
  end

  @impl true
  def init(_) do
    :ets.new(@votes, @ets_opts)
    :ets.new(@candidates, @ets_opts)
    :ets.new(@winners, @ets_opts)

    validators = ValidatorStore.total()
    round = RoundStore.total()
    minimum = calc_minimum(validators)

    load_votes(round, minimum)

    PubSub.subscribe(:verifiers, "block")

    {:ok,
     %{
       validators: validators,
       minimum: minimum,
       round: round,
       validator_id: Global.validator_id()
     }}
  end

  @impl true
  def handle_info(
        {"new_recv", %{id: validator_id},
         %{
           hash: hash,
           prev: _prev,
           height: height,
           round: round,
           creator: creator_id,
           ev_count: ev_count,
           size: _size,
           signature: _signature,
           hashfile: _hashfile,
           timestamp: _timestamp
         } = block},
        %{minimum: minimum, round: round_number, validator_id: me} = state
      )
      when round >= round_number and me != creator_id do
    # Logger.debug("block.new_recv #{Base.encode16(hash)} | from #{validator_id}")

    validator = ValidatorStore.lookup_map(creator_id)

    if BlockTimer.verify(block, validator.pubkey) != :error do
      winner_id = {creator_id, height}
      vote_id = {creator_id, height, validator_id}
      candidate_unique_id = {creator_id, height, hash}

      # emit vote if not exists
      case :ets.insert_new(@votes, {vote_id, round}) do
        true ->
          # retransmit message
          P2P.push_except(["new_recv", block], [validator_id, creator_id])
          # save vote
          BlockStore.insert_vote(creator_id, height, validator_id, round, hash)
          # BlockStore.sync()

          # vote count by candidate
          case :ets.update_counter(
                 @candidates,
                 candidate_unique_id,
                 {4, 1},
                 {candidate_unique_id, round, block, 0}
               ) do
            count when count == minimum ->
              # it's a winner if the count is mayor than minimum required
              case :ets.insert_new(@winners, {winner_id, round}) do
                true ->
                  # create block

                  Task.async(fn ->
                    if ev_count > 0 do
                      # send task to a verifier node
                      push_fetch(self(), block, validator)
                    else
                      # block empty
                      send(BlockTimer, {:import, block})
                    end
                  end)
                  |> Task.await(:infinity)

                _ ->
                  :ok
              end

            _ ->
              :ok
          end

        false ->
          :ok
      end
    end

    {:noreply, state}
  end

  def handle_info(
        {"valid", %{creator: creator, height: height} = block, node_origin},
        state
      ) do
    # Logger.debug("Block.valid #{creator}.#{height}")

    if :ets.member(@winners, {creator, height}) do
      spawn(fn ->
        download_block_from_cluster!(node_origin, creator, height)
        send(BlockTimer, {:import, block})
      end)
    end

    {:noreply, state}
  end

  def handle_info({"invalid", _block}, state) do
    # Logger.debug("Block.invalid #{inspect(block)}")
    # apply block empty with errors
    BlockTimer.put_block_ignore_mine()
    {:noreply, state}
  end

  def handle_info(_msg, state) do
    # Logger.debug("#{__MODULE__} handle_info #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def handle_cast({:validators, n}, %{validators: validators} = state) do
    minimum = calc_minimum(validators)

    {:noreply, %{state | valdiators: validators + n, minimum: minimum}}
  end

  # :ets.fun2ms(fn {_, x, _} when x <= old_round -> true end)
  # :ets.fun2ms(fn {_, x} when x <= old_round -> true end)
  # :ets.fun2ms(fn {_, x, _, _, _} when x <= 10 -> true end)
  @impl true
  def handle_call({:reset, new_round}, _from, %{round: old_round} = state)
      when old_round <= new_round do
    # delete old round
    :ets.select_delete(@winners, [{{:_, :"$1"}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@candidates, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@votes, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])

    {:reply, :ok, %{state | round: new_round}}
  end

  @impl true
  def terminate(_reason, _state) do
    :ets.delete(@votes)
    :ets.delete(@candidates)
    :ets.delete(@winners)
    PubSub.unsubscribe(:verifiers, "block")
  end

  def put_validators(n) do
    GenServer.cast(VoteCounter, {:validators, n})
  end

  # set new round and clear old blocks received
  @spec reset(number()) :: :ok
  def reset(round) do
    GenServer.call(@module, {:reset, round}, :infinity)
  end

  def make_vote(%{hash: hash} = block, validator_id, value) do
    {:ok, signature} = Ippan.Block.sign_vote(hash, value)
    Map.merge(block, %{vote: value, signature: signature, validator_id: validator_id})
  end

  defp download_block_from_cluster!(node_verifier, creator_id, height) do
    hostname = node_verifier |> to_string() |> String.split("@") |> List.last()
    url = Block.cluster_decode_url(hostname, creator_id, height)
    decode_path = Block.decode_path(creator_id, height)
    :ok = Curl.download(url, decode_path)
  end

  defp push_fetch(pid, block, validator), do: push_fetch(pid, block, validator, 1)

  defp push_fetch(pid, block, validator, 0) do
    push_fetch_failed(pid, block, validator)
  end

  defp push_fetch(pid, block, validator, retry) do
    spawn_link(fn ->
      case Node.list() do
        [] ->
          :timer.sleep(1000)
          push_fetch(pid, block, validator, retry - 1)

        node_list ->
          # take random node
          node_atom = Enum.random(node_list)

          case Node.ping(node_atom) do
            :pong ->
              # Logger.debug(inspect("pong block:#{node_atom}"))

              PubSub.direct_broadcast(
                node_atom,
                :verifiers,
                "block",
                {"fetch", block, validator}
              )

            :pang ->
              # Logger.debug("pang")
              :timer.sleep(1000)
              push_fetch(pid, block, validator, retry - 1)
          end
      end
    end)
  end

  defp push_fetch_failed(pid, block, validator) do
    try do
      :ok = BlockTimer.verify_file!(block, validator)
      send(pid, {"valid", block})
    rescue
      _ -> send(pid, {"invalid", block})
    end
  end

  defp load_votes(round, minimum) do
    votes =
      BlockStore.fetch_votes(round)

    # set votes
    Enum.each(votes, fn [creator_id, height, validator_id, round, _hash] ->
      :ets.insert(@votes, {{creator_id, height, validator_id}, round})
    end)

    # set candidates
    Enum.group_by(
      votes,
      fn [creator_id, height, _validator_id, round, hash] ->
        {creator_id, height, hash, round}
      end,
      fn _ ->
        1
      end
    )
    |> Enum.each(fn {{creator_id, height, hash, round}, list_n} ->
      candidate_unique_id = {creator_id, height, hash}

      case :ets.update_counter(
             @candidates,
             candidate_unique_id,
             {4, length(list_n)},
             {candidate_unique_id, round, %{}, 0}
           ) do
        count when count == minimum ->
          # set winners
          :ets.insert(@winners, {{creator_id, height}, round})

        _ ->
          :ok
      end
    end)
  end
end
