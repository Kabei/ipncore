defmodule VoteCounter do
  use GenServer

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
       validators: validators,
       minimum: minimum,
       round: round,
       validator_id: Default.validator_id()
     }}
  end

  @impl true
  def handle_info(
        {:vote,
         %{
           creator: creator_id,
           hash: hash,
           height: height,
           round: round,
           signature: signature,
           validator: validator_id,
           vote: vote
         }, from_pubkey},
        %{minimum: minimum, round: current_round, validator_id: me} = state
      )
      when creator_id != validator_id and vote in [1, -1] and current_round <= round do
    if me == validator_id or
         Cafezinho.Impl.verify(signature, "#{hash}#{vote}", from_pubkey) == :ok do
      winner_id = {creator_id, height}
      vote_id = {creator_id, height, validator_id}
      candidate_unique_id = {creator_id, height, validator_id, hash}

      unless :ets.member(@winners, winner_id) do
        case :ets.insert_new(@votes, {vote_id, round, hash, signature, vote}) do
          true ->
            case :ets.update_counter(
                   @candidates,
                   candidate_unique_id,
                   {2, vote},
                   {candidate_unique_id, round, 1}
                 ) do
              count when count > minimum ->
                case :ets.insert_new(@winners, {winner_id, round}) do
                  true ->
                    t =
                      Task.async(fn ->
                        commit(round)
                        block = BlockMinerChannel.get_block_received(creator_id, height)
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
  def handle_call({:round, n}, _from, %{round: old_round} = state) when old_round < n do
    # delete old round
    :ets.select_delete(@winners, [{{:_, :"$1"}, [{:"=<", :"$1", old_round}], [true]}])
    :ets.select_delete(@candidates, [{{:_, :"$1", :_}, [{:"=<", :"$1", old_round}], [true]}])

    :ets.select_delete(@votes, [
      {{:_, :"$1", :_, :_, :_}, [{:"=<", :"$1", old_round}], [true]}
    ])

    {:reply, :ok, %{state | round: n}}
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

  def put_round(n) do
    GenServer.call(VoteCounter, {:round, n}, :infinity)
  end

  def make_vote(%{hash: hash} = block, value) do
    signature = Ippan.Block.sign_vote(hash, value)
    Map.merge(block, %{vote: value, signature: signature})
  end

  # :ets.fun2ms(fn {_, x, _, _, _} = y when x <= 10 -> y end)
  defp commit(round_number) do
    :ets.select(@votes, [{{:_, :"$1", :_, :_, :_}, [{:==, :"$1", round_number}], [:"$_"]}])
    |> Enum.each(fn {{creator_id, height, validator_id}, round, hash, signature, vote} ->
      BlockStore.insert_vote(height, round, validator_id, creator_id, hash, signature, vote)
    end)
  end
end
