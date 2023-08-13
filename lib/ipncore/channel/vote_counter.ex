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
  @max_block_size Application.compile_env(:ipncore, :max_block_size)

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

    PubSub.subscribe(:cluster, "block")

    {:ok,
     %{
       validators: validators,
       minimum: minimum,
       round: round,
       validator_id: Global.validator_id()
     }, {:continue, :load}}
  end

  @impl true
  def handle_continue(:load, %{round: round} = state) do
    pid = self()

    for [validator_id, data] <- BlockStore.fetch_bft(round) do
      send(pid, {"new_recv", %{id: validator_id}, false, data})
    end

    {:noreply, state}
  end

  @impl true
  def handle_info(
        {"new_recv", %{id: validator_id, pubkey: validator_pubkey}, check,
         %{
           hash: hash,
           prev: prev,
           height: height,
           round: round,
           creator: creator_id,
           ev_count: ev_count,
           size: _size,
           signature: signature,
           hashfile: hashfile,
           timestamp: timestamp
         } = msg},
        %{minimum: minimum, round: round_number, validator_id: me} = state
      )
      when round >= round_number and me != creator_id do
    Logger.debug("block.new_recv #{Base.encode16(hash)} | from #{validator_id}")

    check_auth = creator_id != validator_id
    # is_creator = validator_id == creator_id

    if check == false or
         (Block.compute_hash(height, creator_id, round, prev, hashfile, timestamp) != hash and
            Cafezinho.Impl.verify(signature, hash, validator_pubkey) == :ok and
            (not check_auth or
               Cafezinho.Impl.verify(msg[:auth], "#{hash} is valid", msg[:pubkey]) == :ok)) do
      winner_id = {creator_id, hash}
      vote_id = {creator_id, height, validator_id}
      candidate_unique_id = {creator_id, height, hash}

      # emit vote if not exists
      case :ets.insert_new(@votes, {vote_id, round}) do
        true ->
          if check do
            # retransmit message
            my_auth = Block.sign_block_confirm(hash)

            new_msg =
              msg
              |> Map.put(:auth, my_auth)
              |> Map.put(:pubkey, Global.pubkey())

            P2P.push_except(["new_recv", new_msg], [validator_id, creator_id])
            # save bft message
            BlockStore.insert_bft(validator_id, round, creator_id, height, msg)
          end

          # vote count by candidate
          case :ets.update_counter(
                 @candidates,
                 candidate_unique_id,
                 {4, 1},
                 {candidate_unique_id, round, msg, 0}
               ) do
            count when count == minimum ->
              # it's a winner if the count is mayor than minimum required
              case :ets.insert_new(@winners, {winner_id, nil}) do
                true ->
                  # create block if round is the same
                  if round_number == round do
                    Task.async(fn ->
                      if ev_count > 0 do
                        # send task to a verifier node
                        creator = ValidatorStore.lookup_map(creator_id)
                        push_fetch(self(), msg, creator)
                      else
                        # block empty
                        send(BlockTimer, {:import, msg})
                      end
                    end)
                    |> Task.await(:infinity)
                  end

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

  def handle_cast({:snapshot, %{round: round} = _data}, state) do
    BlockStore.delete_bft(round)

    {:noreply, state}
  end

  # :ets.fun2ms(fn {_, x} when x <= old_round -> true end)
  # :ets.fun2ms(fn {_, x, _} when x <= old_round -> true end)
  @impl true
  def handle_call({:reset, new_round}, _from, %{round: old_round} = state)
      when old_round < new_round do
    # delete old round
    :ets.select_delete(@winners, [{{:_, :"$1"}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@votes, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@candidates, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])

    {:reply, :ok, %{state | round: new_round}}
  end

  @impl true
  def terminate(_reason, _state) do
    :ets.delete(@winners)
    :ets.delete(@votes)
    :ets.delete(@candidates)
    PubSub.unsubscribe(:cluster, "block")
  end

  def put_validators(n) do
    GenServer.cast(VoteCounter, {:validators, n})
  end

  # set new round and clear old blocks received
  @spec reset(number()) :: :ok
  def reset(round) do
    GenServer.call(@module, {:reset, round}, :infinity)
  end

  defp download_block_from_cluster!(node_verifier, creator_id, height) do
    hostname = node_verifier |> to_string() |> String.split("@") |> List.last()
    url = Block.cluster_decode_url(hostname, creator_id, height)
    decode_path = Block.decode_path(creator_id, height)
    :ok = Download.from(url, decode_path, @max_block_size)
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
                :cluster,
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
end
