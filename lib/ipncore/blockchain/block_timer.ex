defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, Round, RequestHandler}
  import Ippan.Block, only: [decode!: 1, encode!: 1, hash_file: 1]
  require Logger

  @otp_app :ipncore
  @pubsub_verifiers :verifiers
  @pubsub_network :network
  @block_interval Application.compile_env(@otp_app, :block_interval)
  @block_max_size Application.compile_env(@otp_app, :block_max_size)
  @block_data_max_size Application.compile_env(@otp_app, :block_data_max_size)
  @block_version Application.compile_env(@otp_app, :block_version)

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    validator_id = Default.validator_id()

    # set state last block
    case BlockStore.last(validator_id) do
      {:row, block_list} ->
        block = Block.to_map(block_list)

        {:ok,
         %{
           height: block.height,
           round: block.round + 1,
           validator_id: validator_id,
           prev_hash: block.hash,
           tRef: nil,
           sync_round: false
         }, {:continue, :start}}

      _ ->
        {:ok, tRef} = :timer.send_after(@block_interval, :mine)

        {:ok,
         %{
           height: 0,
           round: 0,
           validator_id: validator_id,
           prev_hash: nil,
           tRef: tRef,
           sync_round: false
         }}
    end
  end

  @impl true
  def handle_continue(:start, %{round: round} = state) do
    round_id = RoundStore.last_id()

    if round_id == round do
      {:ok, tRef} = :timer.send_after(@block_interval, :mine)
      {:noreply, Map.put(state, :tRef, tRef)}
    else
      {:noreply, state}
    end
  end

  def mine do
    pid = Process.whereis(BlockTimer)
    send(pid, :mine)
  end

  def next_round do
    GenServer.cast(BlockTimer, :next_round)
  end

  def start do
    GenServer.cast(BlockTimer, :start)
  end

  def get_round do
    GenServer.call(BlockTimer, :round, :infinity)
  end

  def get_height do
    GenServer.call(BlockTimer, :height, :infinity)
  end

  @impl true
  def handle_cast(:start, state) do
    {:ok, tRef} = :timer.send_after(@block_interval, :mine)
    {:noreply, Map.put(state, :tRef, tRef)}
  end

  def handle_cast(:next_round, %{sync_round: false} = state) do
    spawn_link(fn ->
      round_build(state)
    end)

    {:noreply, %{state | sync_round: true}}
  end

  def handle_cast(_, state) do
    {:noreply, state}
  end

  @impl true
  def handle_call(:round, _from, %{round: round} = state) do
    {:reply, round, state}
  end

  def handle_call(:height, _from, %{height: height} = state) do
    {:reply, height, state}
  end

  @doc """
  Fetch requests and send to mine
  Keeps height and prev_hash up to date
  """
  @impl true
  def handle_info(
        :mine,
        %{
          height: height,
          round: old_round,
          validator_id: validator_id,
          prev_hash: prev_hash
        } = state
      ) do
    :timer.cancel(state.tRef)
    {:ok, requests} = MessageStore.select(@block_data_max_size, validator_id)
    {:ok, requests_df} = MessageStore.select_df(@block_data_max_size, validator_id)

    last_row_id = catch_last_row_id(requests)

    last_row_id_df = catch_last_row_id(requests_df)

    {new_height, new_hash} =
      mine(
        requests ++ requests_df,
        height,
        old_round,
        validator_id,
        prev_hash
      )

    if new_height != height do
      MessageStore.delete_all(last_row_id)
      MessageStore.delete_all_df(last_row_id_df)
      MessageStore.sync()

      {:noreply, %{state | height: new_height, prev_hash: new_hash}}
    else
      {:noreply, state}
    end
  end

  def handle_info({:end, new_state}, state) do
    {:noreply, Map.merge(state, new_state)}
  end

  @impl true
  def terminate(_reason, %{tRef: tRef}) do
    :timer.cancel(tRef)
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  def round_build(%{round: round}) do
    {:ok, requests} = MessageStore.delete_all_df_approved(round)

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

    {:ok, result_hashes} = BlockStore.fetch_round(round)
    hashes = Enum.concat(result_hashes)
    count = length(hashes)
    hash = Round.compute_hash(round, hashes)
    {:row, [time]} = BlockStore.avg_round_time(round)

    RoundStore.insert([round, hash, count, trunc(time)])

    MessageStore.sync()

    if requests != [] do
      sync_all()
    end

    RoundStore.sync()
    checkpoint_all()

    # Clear cache
    new_round = round + 1
    BlockMinerChannel.new_round(new_round)
    PubSub.broadcast(:verifiers, "round", {"end", round})

    # Start new timer to mine
    {:ok, tRef} = :timer.send_after(@block_interval, :mine)

    send(BlockTimer, {:end, %{round: new_round, tRef: tRef, sync_round: false}})
  end

  defp catch_last_row_id([]), do: -1

  defp catch_last_row_id(requests) do
    List.last(requests)
    |> List.last()
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

  def mine_file(
        %{creator: creator_id, height: height, prev: prev_hash, round: round},
        decode_path
      ) do
    {:ok, content} = File.read(decode_path)

    requests = decode!(content)

    mine(requests, height, round, creator_id, prev_hash)
  end

  def mine(requests, height, round, validator_id, prev_hash) do
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

    content = encode!(events)

    :ok = File.write(block_path, content)
    block_size = File.stat!(block_path).size
    hashfile = hash_file(block_path)
    timestamp = :os.system_time(:millisecond)
    ev_count = length(events)

    if ev_count > 0 do
      sync_all()
    end

    block_map = %{
      height: height,
      prev: prev_hash,
      creator: validator_id,
      hashfile: hashfile,
      round: round,
      timestamp: timestamp,
      ev_count: ev_count,
      size: block_size,
      vsn: @block_version
    }

    blockhash = Block.compute_hash(block_map)
    {:ok, signature} = Block.sign(blockhash)
    block_map = Map.merge(block_map, %{hash: blockhash, signature: signature})

    Block.to_list(block_map)
    |> BlockStore.insert_sync()

    BlockStore.sync()

    Logger.debug("Block #{height} | events: #{ev_count} | hash: #{Base.encode16(blockhash)}")
    PubSub.broadcast(@pubsub_verifiers, "block", {"new", block_map})
    PubSub.broadcast(@pubsub_network, "msg", {"block", "new_recv", block_map})

    {height + 1, blockhash}
  end

  defp sync_all do
    Logger.debug("sync all")
    WalletStore.sync()
    BalanceStore.sync()
    ValidatorStore.sync()
    TokenStore.sync()
    DomainStore.sync()
    DnsStore.sync()
    EnvStore.sync()
    RefundStore.sync()
  end

  defp checkpoint_all do
    Logger.debug("checkpoint all")
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

  defp decode_term(nil), do: nil

  defp decode_term(x) do
    :erlang.binary_to_term(x)
  end
end
