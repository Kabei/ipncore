defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, Round, RequestHandler}
  import Ippan.Block, only: [decode!: 1, encode!: 1, hash_file: 1, put_hash: 1, put_signature: 1]
  require Logger

  @otp_app :ipncore
  @module __MODULE__
  @token Application.compile_env(:ipncore, :token)
  @time_activity Application.compile_env(:ipncore, :last_activity)
  @pubsub_verifiers :verifiers
  @pubsub_network :network
  @topic_block "block"
  @topic_round "round"
  @topic_network "msg"
  @block_interval Application.compile_env(@otp_app, :block_interval)
  @block_max_size Application.compile_env(@otp_app, :block_max_size)
  @block_data_max_size Application.compile_env(@otp_app, :block_data_max_size)
  @block_version Application.compile_env(@otp_app, :block_version)

  def start_link(opts) do
    GenServer.start_link(@module, opts, name: @module, hibernate_after: 5_000)
  end

  @impl true
  def init(_args) do
    validator_id = Default.validator_id()

    {round_id, round_hash} =
      case RoundStore.last() do
        {:row, [round_id, hash | _]} -> {round_id + 1, hash}
        _ -> {0, nil}
      end

    # set state last block
    {block_id, block_hash, block_round} =
      case BlockStore.last(validator_id) do
        {:row, [id, _creator, hash, _prev, _hashfile, _sig, block_round | _rest]} ->
          {id + 1, hash, block_round}

        _ ->
          {0, nil}
      end

    {:ok,
     %{
       block_sync: false,
       next_block: block_id + 1,
       next_round: round_id + 1,
       prev_block: block_hash,
       prev_round: round_hash,
       round_sync: false,
       wait_to_mine: false,
       wait_to_sync: false,
       validator_id: validator_id,
       tRef: nil
     }, {:continue, {:continue, block_round}}}
  end

  @impl true
  def handle_continue(
        {:continue, block_round},
        %{next_round: next_round} = state
      ) do
    if block_round == next_round do
      blocks = BlockStore.count_by_round(block_round)
      total = ValidatorStore.total()

      if total == blocks do
        send(self(), :sync)
        {:noreply, state}
      else
        {:ok, tRef} = :timer.send_after(@block_interval, :mine)
        {:noreply, Map.put(state, :tRef, tRef)}
      end
    end
  end

  # build a round
  def sync do
    GenServer.cast(@module, :sync)
  end

  @impl true
  def handle_cast({:complete, :block, new_height, new_hash}, state) do
    if state.wait_to_sync do
      GenServer.cast(self(), :sync)
    end

    {:noreply, %{state | next_block: new_height, prev_block: new_hash, block_sync: false}}
  end

  def handle_cast({:complete, :round, new_id, new_hash}, state) do
    BlockMinerChannel.new_round(new_id)
    PubSub.broadcast(@pubsub_verifiers, @topic_round, {"start", new_id})

    tRef =
      if state.wait_to_mine do
        GenServer.cast(self(), :mine)
      else
        {:ok, tRef} = :timer.send_after(@block_interval, :mine)
        tRef
      end

    {:noreply, %{state | next_round: new_id, prev_round: new_hash, round_sync: false, tRef: tRef}}
  end

  @impl true
  def handle_call(:round, _from, %{next_round: id} = state) do
    {:reply, id, state}
  end

  def handle_call(:height, _from, %{next_block: id} = state) do
    {:reply, id, state}
  end

  @doc """
  Fetch requests and send to mine
  Keeps height and prev_hash up to date
  """
  @impl true
  def handle_info(
        :mine,
        %{
          block_sync: false,
          round_sync: false,
          tRef: tRef
        } = state
      ) do
    :timer.cancel(tRef)
    spawn_block_worker(self(), state)
    {:noreply, %{state | block_sync: true}}
  end

  def handle_info(:mine, state) do
    {:noreply, %{state | wait_to_mine: true}}
  end

  def handle_info(
        :sync,
        %{
          block_sync: false,
          round_sync: false,
          tRef: tRef
        } = state
      ) do
    :timer.cancel(tRef)
    spawn_round_worker(self(), state)
    {:noreply, %{state | round_sync: true}}
  end

  def handle_info(:sync, state) do
    {:noreply, %{state | wait_to_sync: true}}
  end

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  defp spawn_block_worker(
         pid,
         %{
           next_block: next_block,
           next_round: next_round,
           prev_block: prev_block,
           validator_id: validator_id
         }
       ) do
    spawn_link(fn ->
      {:ok, requests} = MessageStore.select(@block_data_max_size, validator_id)
      {:ok, requests_df} = MessageStore.select_df(@block_data_max_size, validator_id)

      last_row_id = catch_last_row_id(requests)

      last_row_id_df = catch_last_row_id(requests_df)

      new_hash =
        mine_fun(
          requests ++ requests_df,
          next_block,
          next_round,
          validator_id,
          prev_block,
          :os.system_time(:millisecond)
        )

      MessageStore.delete_all(last_row_id)
      MessageStore.delete_all_df(last_row_id_df)
      MessageStore.sync()

      GenServer.cast(pid, {:complete, :block, next_block + 1, new_hash})
    end)
  end

  defp spawn_round_worker(pid, %{next_round: next_round, prev_round: prev_round}) do
    spawn_link(fn ->
      {:ok, requests} = MessageStore.delete_all_df_approved(next_round)

      Enum.each(requests, fn [
                               _key,
                               type,
                               timestamp,
                               hash,
                               account_id,
                               validator_id,
                               args,
                               _message,
                               _signature,
                               size
                             ] ->
        RequestHandler.handle_post!(
          hash,
          type,
          timestamp,
          account_id,
          validator_id,
          size,
          decode_term(args)
        )
      end)

      {:ok, result_hashes} = BlockStore.fetch_round(next_round)
      hashes = Enum.concat(result_hashes)
      count = length(hashes)
      hash = Round.compute_hash(next_round, prev_round, hashes)
      {:row, [timestamp]} = BlockStore.avg_round_time(next_round)

      RoundStore.insert([next_round, hash, prev_round, count, timestamp])

      MessageStore.sync()

      if requests != [] do
        commit()
      end

      run_jackpot(next_round, hash, timestamp)

      RoundStore.sync()
      checkpoint_commit()

      # send pubsub event
      PubSub.broadcast(@pubsub_verifiers, @topic_round, {"end", next_round})

      GenServer.cast(pid, {:complete, :round, next_round + 1, hash})
    end)
  end

  # Create a block file from decode block file
  @spec mine_file(Block.t(), Path.t()) :: binary()
  def mine_file(
        %{
          creator: creator_id,
          height: height,
          prev: prev_hash,
          round: round,
          timestamp: timestamp
        },
        decode_path
      ) do
    {:ok, content} = File.read(decode_path)

    requests = decode!(content)

    mine_fun(requests, height, round, creator_id, prev_hash, timestamp)
  end

  # Create a block file and register transactions
  defp mine_fun(requests, height, round, validator_id, prev_hash, block_timestamp) do
    block_path = Block.block_path(validator_id, height)

    events =
      Enum.reduce(requests, [], fn
        [
          timestamp,
          hash,
          type,
          account_id,
          validator_id,
          args,
          message,
          signature,
          size,
          _rowid
        ],
        acc ->
          try do
            args = decode_term(args)

            RequestHandler.handle!(
              hash,
              type,
              timestamp,
              account_id,
              validator_id,
              size,
              args,
              round
            )

            acc ++ [{message, signature}]
          rescue
            # block failed
            e ->
              Logger.debug(Exception.format(:error, e, __STACKTRACE__))
              acc
          end

        [
          _key,
          type,
          timestamp,
          hash,
          account_id,
          validator_id,
          args,
          message,
          signature,
          size,
          _rowid
        ],
        acc ->
          try do
            args = decode_term(args)

            RequestHandler.handle!(
              hash,
              type,
              timestamp,
              account_id,
              validator_id,
              size,
              args,
              round
            )

            acc ++ [{message, signature}]
          rescue
            # block failed
            e ->
              Logger.debug(Exception.format(:error, e, __STACKTRACE__))
              acc
          end
      end)

    ev_count = length(events)
    empty = ev_count == 0

    {hashfile, block_size} =
      if empty do
        {Block.zero_hash_file(), 0}
      else
        content = encode!(events)
        hashfile = hash_file(block_path)
        :ok = File.write(block_path, content)
        block_size = File.stat!(block_path).size
        commit()
        {hashfile, block_size}
      end

    block =
      %{
        height: height,
        prev: prev_hash,
        creator: validator_id,
        hashfile: hashfile,
        round: round,
        timestamp: block_timestamp,
        ev_count: ev_count,
        size: block_size,
        vsn: @block_version
      }
      |> put_hash()
      |> put_signature()

    Block.to_list(block)
    |> BlockStore.insert_sync()

    BlockStore.sync()

    Logger.debug("Block #{height} | events: #{ev_count} | hash: #{Base.encode16(block.hash)}")

    PubSub.broadcast(@pubsub_verifiers, @topic_block, {"new", block})
    PubSub.broadcast(@pubsub_network, @topic_network, {"block", "new_recv", block})

    block.hash
  end

  @doc """
  used by verifiers to download and verify blockfiles
  """
  def verify_block!(block, validator) do
    block_path = Block.block_path(validator.id, block.height)
    filename = Path.basename(block_path)
    url = Block.url(validator.hostname, validator.id, block.height)

    {:ok, _} = Curl.download_block(url, block_path)
    {:ok, filestat} = File.stat(block_path)

    if filestat.size > @block_max_size do
      raise IppanError, "Invalid block max size"
    end

    {:ok, content} = File.read(block_path)

    block_hash = hash_file(block_path)

    if block.hashfile != block_hash do
      raise(IppanError, "Hash block file is invalid")
    end

    if Cafezinho.Impl.verify(block.signature, block.hash, validator.pubkey) != :ok do
      raise(IppanError, "Invalid block signature")
    end

    events = decode!(content)

    decode_events =
      for {body, signature} <- events do
        hash = Blake3.hash(body)
        size = byte_size(body) + byte_size(signature)

        RequestHandler.valid!(hash, body, size, signature, validator.id)
      end

    export_path =
      Application.get_env(@otp_app, :decode_dir)
      |> Path.join(filename)

    :ok = File.write(export_path, encode!(decode_events))

    export_path
  end

  # Pay a player according to the calculation of the remaining
  # hash of the round ordered by activity in the last 24 hours
  defp run_jackpot(round_id, hash, round_timestamp) do
    time_activity = round_timestamp - @time_activity

    num =
      :binary.part(hash, 24, 32)
      |> :binary.decode_unsigned()

    count = BalanceStore.count_last_activity(time_activity)

    if count > 0 do
      position = rem(num, count)
      {:ok, players} = BalanceStore.last_activity(time_activity)
      [winner_id] = Enum.at(players, position)
      amount = EnvStore.get("WINNER_AMOUNT", 10) * count
      RoundStore.insert_winner(round_id, winner_id)
      BalanceStore.income(winner_id, @token, amount, round_timestamp)
    else
      RoundStore.insert_winner(round_id, nil)
    end
  end

  defp commit do
    Logger.debug("commit")
    WalletStore.sync()
    BalanceStore.sync()
    ValidatorStore.sync()
    TokenStore.sync()
    DomainStore.sync()
    DnsStore.sync()
    EnvStore.sync()
    RefundStore.sync()
  end

  defp checkpoint_commit do
    Logger.debug("checkpoint_commit")
    WalletStore.checkpoint()
    BalanceStore.checkpoint()
    ValidatorStore.checkpoint()
    TokenStore.checkpoint()
    DomainStore.checkpoint()
    DnsStore.checkpoint()
    EnvStore.checkpoint()
    RefundStore.checkpoint()
    BlockStore.checkpoint()
    RoundStore.checkpoint()
    MessageStore.checkpoint()
  end

  defp catch_last_row_id([]), do: -1

  defp catch_last_row_id(requests) do
    List.last(requests)
    |> List.last()
  end

  defp decode_term(nil), do: nil

  defp decode_term(x) do
    :erlang.binary_to_term(x)
  end
end
