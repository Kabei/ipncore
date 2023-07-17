defmodule VoteCounter do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, P2P}
  require Logger

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

  @impl true
  def init(_) do
    :ets.new(@votes, @ets_opts)
    :ets.new(@candidates, @ets_opts)
    :ets.new(@winners, @ets_opts)

    validators = ValidatorStore.total()
    round = RoundStore.total()

    minimum =
      if validators > 2 do
        div(validators, 2) + 1
      else
        0
      end

    {:ok,
     %{
       blocks: %{},
       validators: validators,
       minimum: minimum,
       round: round,
       validator_id: Default.validator_id()
     }}
  end

  @impl true
  def handle_info(
        {"new_recv", from,
         %{
           hash: hash,
           height: height,
           round: round,
           creator: creator_id,
           ev_count: ev_count
         } = block},
        %{round: round_number} = state
      )
      when round >= round_number do
    Logger.debug("block.new_recv #{Base.encode16(hash)}")

    block_unique_id = {creator_id, height}
    candidate_unique_id = {creator_id, height, hash}

    result = :ets.lookup(@candidates, candidate_unique_id)

    if result == [] or round > round_number do
      Logger.debug("not in cache")

      :ets.insert_new(@candidates, {candidate_unique_id, round, block, 0})
      # check if a winner
      case :ets.member(@winners, block_unique_id) do
        false ->
          if ev_count > 0 do
            # send task to a verifier node
            push_fetch(block)
          else
            # block empty
            vote =
              try do
                :ok = BlockTimer.verify_empty!(block, from)

                make_vote(block, Default.validator_id(), 1)
              rescue
                _ -> make_vote(block, Default.validator_id(), -1)
              end

            # send vote
            send(VoteCounter, {"vote", vote, nil})
          end

        true ->
          nil
      end

      {:noreply, state}
    else
      Logger.debug("hit cache #{block.creator}.#{block.height}")

      :ets.update_element(@candidates, candidate_unique_id, {2, block})
      {:noreply, state}
    end
  end

  def handle_info(
        {"vote",
         %{
           creator: creator_id,
           hash: hash,
           height: height,
           round: round,
           signature: signature,
           validator_id: validator_id,
           vote: vote
         }, from_pubkey},
        %{minimum: minimum, round: current_round, validator_id: me} = state
      )
      when creator_id != validator_id and vote in [1, -1] and current_round <= round do
    Logger.debug("vote #{creator_id}.#{height} | #{round}: #{vote}")
    # check signature, but it's me no check signature
    if me == validator_id or
         Cafezinho.Impl.verify(signature, "#{hash}#{vote}", from_pubkey) == :ok do
      winner_id = {creator_id, height}
      vote_id = {creator_id, height, validator_id}
      candidate_unique_id = {creator_id, height, hash}

      # check winner
      unless :ets.member(@winners, winner_id) do
        # insert voter
        case :ets.insert_new(@votes, {vote_id, round, hash, signature, vote}) do
          true ->
            # vote count by candidate
            case :ets.update_counter(
                   @candidates,
                   candidate_unique_id,
                   {2, vote},
                   {candidate_unique_id, round, 1}
                 ) do
              count when count > minimum ->
                # it's a winner if the count is mayor than minimum required
                case :ets.insert_new(@winners, {winner_id, round}) do
                  true ->
                    # send a task to save votes
                    t =
                      Task.async(fn ->
                        commit(round)

                        [{_id, _round, block, _count}] =
                          :ets.lookup(@candidates, candidate_unique_id)

                        send(BlockTimer, {:import, block})
                      end)

                    Task.await(t, :infinity)

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
    end

    {:noreply, state}
  end

  def handle_info(
        {"valid", %{creator: creator, height: height} = vote, node_origin},
        state
      ) do
    send(VoteCounter, {:vote, vote, nil})
    P2P.push({"vote", vote})

    spawn(fn ->
      download_block_from_cluster!(node_origin, creator, height)
    end)

    {:noreply, state}
  end

  def handle_info({"invalid", vote}, state) do
    P2P.push({"vote", vote})

    {:noreply, state}
  end

  def handle_info(msg, state) do
    Logger.debug("#{__MODULE__} handle_info #{inspect(msg)}")
    {:noreply, state}
  end

  @impl true
  def handle_cast({:validators, n}, %{validators: validators} = state) do
    minimum =
      if n > 2 do
        div(n, 2) + 1
      else
        0
      end

    {:noreply, %{state | valdiators: validators + n, minimum: minimum}}
  end

  # :ets.fun2ms(fn {_, x, _} when x <= old_round -> true end)
  # :ets.fun2ms(fn {_, x} when x <= old_round -> true end)
  # :ets.fun2ms(fn {_, x, _, _, _} when x <= 10 -> true end)
  @impl true
  def handle_call({:reset, new_round}, _from, %{round: old_round} = state)
      when old_round < new_round do
    # delete old round
    :ets.select_delete(@winners, [{{:_, :"$1"}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@candidates, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])

    :ets.select_delete(@votes, [
      {{:_, :"$1", :_, :_, :_}, [{:"=<", :"$1", old_round}], [true]}
    ])

    {:reply, :ok, %{state | round: new_round}}
  end

  @impl true
  def terminate(_reason, _state) do
    :ets.delete(@votes)
    :ets.delete(@candidates)
    :ets.delete(@winners)
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
    spawn(fn ->
      hostname = node_verifier |> to_string() |> String.split("@") |> List.last()
      url = "http://#{hostname}:8080/v1/download/block-decode/#{creator_id}/#{height}"
      decode_path = Block.decode_path(creator_id, height)
      {:ok, _abs_url} = Curl.download(url, decode_path)
    end)
  end

  defp push_fetch(block) do
    spawn_link(fn ->
      case Node.list() do
        [] ->
          :timer.sleep(1500)
          push_fetch(block)

        node_list ->
          # take random node
          node_atom = Enum.random(node_list)

          case Node.ping(node_atom) do
            :pong ->
              Logger.debug(inspect("pong block:#{node_atom}"))
              validator = ValidatorStore.lookup([block.creator])

              PubSub.direct_broadcast(
                node_atom,
                :verifiers,
                "block",
                {"fetch", block, validator}
              )

            :pang ->
              Logger.debug("pang")
              :timer.sleep(1500)
              push_fetch(block)
          end
      end
    end)
  end

  # :ets.fun2ms(fn {_, x, _} = y when x <= 10 -> y end)
  defp commit(round_number) do
    :ets.select(@votes, [{{:_, :"$1", :_}, [{:"=<", :"$1", round_number}], [:"$_"]}])
    |> Enum.each(fn {{creator_id, height, validator_id}, round, hash, signature, vote} ->
      BlockStore.insert_vote(height, round, validator_id, creator_id, hash, signature, vote)
    end)
  end
end
