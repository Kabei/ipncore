defmodule BlockMinerChannel do
  use Channel,
    server: :verifiers,
    channel: "block"

  alias Ippan.{Block, P2P}

  def init(_args) do
    PubSub.subscribe(@pubsub_server, @channel)

    round_id = RoundStore.last_id()
    {:ok, %{blocks: %{}, round: round_id}}
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
        %{blocks: blocks, round: round_number} = state
      ) do
    Logger.debug("block.new_recv #{Base.encode16(hash)}")

    block_unique_id = {creator_id, height}

    if not Map.has_key?(block_unique_id, blocks) or round > round_number do
      Logger.debug("not in cache")

      if ev_count > 0 do
        push_fetch(block)
      else
        try do
          BlockTimer.verify!(block, from)
        rescue
          _ ->
            send(self(), {"valid", :error, block})
        end
      end

      {:noreply, %{state | blocks: Map.put(blocks, block_unique_id, block)}}
    else
      Logger.debug("hit cache #{block.creator}.#{block.height}")
      {:noreply, state}
    end
  end

  def handle_info(
        {"valid", %{creator: creator, height: height, vote: value} = vote, node_origin},
        state
      ) do
    send(VoteCounter, {:vote, vote, nil})
    P2P.push({"vote", vote})

    if value == 1 do
      spawn(fn ->
        download_block_from_cluster!(node_origin, creator, height)
      end)
    end

    {:noreply, state}
  end

  def handle_info({"vote", from, block}, state) do
    send(VoteCounter, {:vote, block, from.pubkey})

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_call({:block, creator_id, height}, _from, %{blocks: blocks} = state) do
    {:reply, Map.get(blocks, {blocks, creator_id, height}), state}
  end

  @impl true
  def handle_cast({:reset, new_round}, %{blocks: blocks, round: old_round} = state) do
    result =
      Enum.reduce(blocks, %{}, fn {key, %{round: round} = block}, acc ->
        if round != old_round do
          Map.put(acc, key, block)
        else
          acc
        end
      end)

    {:noreply, %{state | blocks: result, round: new_round}}
  end

  # set new round and clear old blocks received
  @spec reset(number()) :: :ok
  def reset(round) do
    GenServer.cast(@module, {:reset, round})
  end

  def get_block_received(creator_id, height) do
    GenServer.call(@module, {:block, creator_id, height})
  end

  defp download_block_from_cluster!(node_verifier, creator_id, height) do
    hostname = node_verifier |> to_string() |> String.split("@") |> List.last()
    url = "http://#{hostname}:8080/v1/download/block-decode/#{creator_id}/#{height}"
    decode_path = Block.decode_path(creator_id, height)
    {:ok, _abs_url} = Curl.download(url, decode_path)
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
end
