defmodule BlockChannel do
  use GenServer
  alias Phoenix.PubSub

  @module __MODULE__
  @pubsub_server :network
  @channel "block"

  @otp_app :ipncore
  @file_extension "erl"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, hibernate_after: 1_000, name: @module)
  end

  def push(message) do
    GenServer.cast(@module, {:push, message})
  end

  @impl true
  def init(args) do
    PubSub.subscribe(@pubsub_server, @channel)
    {:ok, args}
  end

  @impl true
  def handle_info(
        {"fetch",
         %{
           creator: vid,
           hostname: hostname,
           height: height
         } = block},
        state
      ) do
    data_dir = Application.get_env(@otp_app, :data_dir)
    filename = "#{vid}.#{height}.#{@file_extension}"
    block_path = Path.join(data_dir, "blocks-decode/#{filename}")

    url = "https://#{hostname}/v1/download/blocks-decode/#{filename}"
    Download.from(url, path: block_path)

    try do
      validator = ValidatorStore.lookup([vid])
      result_path = BlockTimer.verify_block!(block, validator)

      PubSub.broadcast(
        :miner,
        "block",
        {"valid-file", Map.merge(block, %{status: :ok, path: result_path, validator: validator})}
      )
    rescue
      _ -> PubSub.broadcast(:miner, "block", {"valid-file", Map.put(block, :status, :error)})
    end

    {:noreply, state}
  end

  def handle_info(
        {"new-recv",
         %{hash: hash, height: height, round: round, creator: creator_id, signature: signature} =
           block},
        state
      ) do
    node_name = get_random_verifier()
    BlockStore.insert_vote(height, round, creator_id, creator_id, signature, hash, 0)
    PubSub.broadcast_from(:verifiers, self(), "block:#{node_name}", {"fetch", block})
    {:noreply, state}
  end

  def handle_info(
        {"recv",
         %{
           hash: hash,
           height: height,
           round: round,
           vote: vote,
           creator: creator_id,
           signature: signature,
           prev: prev,
           validator: validator_id
         } = block},
        %{min_votes: min_votes} = state
      ) do
    if validator_id != creator_id and vote in [1, -1] do
      validator = ValidatorStore.lookup([validator_id])

      if Cafezinho.Impl.verify(signature, "#{hash}#{vote}", validator.pubkey) == :ok do
        BlockStore.insert_vote(height, round, validator_id, creator_id, signature, hash, vote)

        {:row, [sum_votes, total_votes]} = BlockStore.sum_votes(round)

        if sum_votes == 3 and total_votes == min_votes do
          {:rwo, [prev_hash]} = BlockStore.last(creator_id)

          if prev_hash == prev do
            BlockTimer.mine_file(block)
          end
        end
      end
    end

    {:noreply, state}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_cast({:push, message}, state) do
    PubSub.broadcast_from(@pubsub_server, self(), @channel, message)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_server, @channel)
  end

  defp get_random_verifier do
    Node.list() |> Enum.random() |> to_string() |> String.split("@") |> hd
  end
end
