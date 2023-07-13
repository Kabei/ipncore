defmodule BlockMinerChannel do
  use Channel,
    server: :miner,
    channel: "block"

  alias Ippan.{Block}

  def init(_args) do
    PubSub.subscribe(@pubsub_server, @channel)

    round_id = RoundStore.last_id()
    {:ok, %{cache: [], round: round_id}}
  end

  def new_round(round) do
    GenServer.cast(@module, {:new_round, round})
  end

  @impl true
  def handle_info(
        {"new_recv", from_id,
         %{
           hash: hash,
           height: height,
           round: round,
           creator: creator_id,
           signature: signature
         } = block},
        %{cache: cache} = state
      ) do
    Logger.debug("block.new_recv #{Base.encode16(hash)}")

    if hash not in cache do
      Logger.debug("not cache")
      BlockStore.insert_vote(height, round, from_id, creator_id, hash, signature, 0)
      send_fetch(self(), block)
      {:noreply, %{state | cache: cache ++ [hash]}}
    else
      Logger.debug("hit cache")
      {:noreply, state}
    end
  end

  def handle_info({"valid", :ok, %{hash: hash, round: round} = data, origin}, state) do
    {:ok, signature} = Block.sign("#{hash}1")

    vote = register_vote(data, signature, 1)
    download_and_process(origin, data)
    PubSub.broadcast(:network, "msg", {"block", "vote", vote})

    {_, [total]} = ValidatorStore.total()
    {_, [count]} = BlockStore.count_by_round(round)

    if count == total do
      BlockTimer.round_end()
    end

    {:noreply, state}
  end

  def handle_info({"valid", :error, data}, state) do
    {:ok, signature} = Block.sign_vote(data.hash, 1)
    vote = register_vote(data, signature, 1)

    PubSub.broadcast(:network, "msg", {"block", "vote", vote})
    {:noreply, state}
  end

  def handle_info(
        {"vote",
         %{
           hash: hash,
           height: _height,
           round: _round,
           vote: vote,
           creator: creator_id,
           signature: signature,
           prev: _prev,
           validator: validator_id
         } = block},
        state
      ) do
    if validator_id != creator_id and vote in [1, -1] do
      validator = ValidatorStore.lookup([validator_id])

      if Cafezinho.Impl.verify(signature, "#{hash}#{vote}", validator.pubkey) == :ok do
        register_vote(block, signature, vote)

        # {:row, [sum_votes, total_votes]} = BlockStore.sum_votes(round, hash, creator_id)

        # if sum_votes == 3 and total_votes == min_votes do
        #   {:row, [prev_hash]} = BlockStore.last(creator_id)

        #   if prev_hash == prev do
        #     filename = "#{creator_id}.#{height}.#{@file_extension}"
        #     decode_dir = Application.get_env(@otp_app, :decode_dir)
        #     block_path = Path.join([decode_dir, filename])
        #     BlockTimer.mine_file(block, block_path)
        #   end
        # end
      end
    end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:new_round, round}, state) do
    {:noreply, %{state | cache: [], round: round}}
  end

  defp download_and_process(
         hostname,
         %{creator: creator_id, height: height} = block
       ) do
    decode_path = Block.block_path(creator_id, height)
    block_path = Block.block_path(creator_id, height)
    filename = Path.basename(decode_path)

    url = "http://#{hostname}:8080/v1/download/block-decode/#{filename}"

    {:ok, _abs_url} = Download.from(url, path: decode_path)

    BlockTimer.mine_file(block, block_path)
  end

  defp register_vote(
         %{
           height: height,
           round: round,
           creator: creator_id,
           hash: hash,
           validator: validator_id
         } = block,
         signature,
         vote
       ) do
    BlockStore.insert_vote(
      height,
      round,
      validator_id,
      creator_id,
      hash,
      signature,
      vote
    )

    Map.put(block, :signature, signature)
  end

  defp send_fetch(pid, block) do
    spawn_link(fn ->
      case Node.list() do
        [] ->
          :timer.sleep(1500)
          send_fetch(pid, block)

        node_list ->
          node_atom = node_list |> Enum.random()

          local = node() |> to_string() |> String.split("@") |> List.last()
          Logger.debug(inspect(local))

          case Node.ping(node_atom) do
            :pong ->
              Logger.debug(inspect("pong block:#{node_atom}"))
              PubSub.subscribe(:miner, "block:#{block.hash} #{inspect(block)}")
              validator = ValidatorStore.lookup([block.creator])

              PubSub.broadcast(
                :verifiers,
                "block:#{node_atom}",
                {"fetch", %{hostname: validator.hostname, origin: local}, block}
              )

              receive do
                {"valid", _result, _block, host} = msg when local == host ->
                  PubSub.unsubscribe(:miner, "block:#{block.hash}")
                  send(pid, msg)

                _ ->
                  :ok
              after
                10_000 ->
                  PubSub.unsubscribe(:miner, "block:#{block.hash}")
                  send_fetch(pid, block)
              end

            :pang ->
              Logger.debug("pang")
              :timer.sleep(1500)
              send_fetch(pid, block)
          end
      end
    end)
  end
end
