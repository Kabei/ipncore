defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, Round, RequestHandler}
  require Logger

  @otp_app :ipncore
  @pubsub_verifiers :verifiers
  @pubsub_network :network
  @block_interval Application.compile_env(@otp_app, :block_interval)
  @block_max_size Application.compile_env(@otp_app, :block_max_size)
  @block_version Application.compile_env(@otp_app, :block_version)
  @file_extension "erl"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    # create folders
    data_dir = Application.get_env(@otp_app, :data_dir)
    block_dir = Path.join(data_dir, "blocks")
    block_decode_dir = Path.join(data_dir, "blocks-decode")
    File.mkdir(block_dir)
    File.mkdir(block_decode_dir)

    validator_id = Default.validator_id()

    # set state last block
    last_state(validator_id)
  end

  defp last_state(validator_id) do
    case BlockStore.last(validator_id) do
      {:row, block_list} ->
        block = Block.to_map(block_list)

        {:ok,
         %{
           height: block.height,
           round: block.round,
           validator_id: validator_id,
           prev_hash: block.hash
         }}

      _ ->
        {:ok,
         %{
           height: 0,
           round: 0,
           validator_id: validator_id,
           prev_hash: nil
         }}
    end
  end

  def mine do
    pid = Process.whereis(BlockTimer)
    send(pid, :mine)
  end

  def round_end do
    GenServer.call(BlockTimer, :round_end, :infinity)
  end

  def start do
    GenServer.cast(BlockTimer, :init)
  end

  def get_round do
    GenServer.call(BlockTimer, :round, :infinity)
  end

  def get_height do
    GenServer.call(BlockTimer, :height, :infinity)
  end

  @impl true
  def handle_call(:round, _from, %{round: round} = state) do
    {:reply, round, state}
  end

  def handle_call(:height, _from, %{height: height} = state) do
    {:reply, height, state}
  end

  def handle_call(:round_end, _from, %{round: round} = state) do
    {:ok, requests} = MessageStore.delete_all_df_approved(round)
    time = :os.system_time(:millisecond)

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

    {:ok, [hashes]} = BlockStore.fetch_round(round)

    count = length(hashes)
    hash = Round.compute_hash(round, hashes)
    RoundStore.insert([round, hash, count, time])

    sync_all()
    MessageStore.sync()
    RoundStore.sync()
    checkpoint_all()

    {:reply, round, %{state | round: round + 1}}
  end

  @impl true
  def handle_cast(:start, state) do
    tref = :timer.send_after(@block_interval, :mine)
    {:noreply, Map.put(state, :tref, tref)}
  end

  @impl true
  def handle_info(
        :mine,
        %{height: height, round: old_round, validator_id: validator_id, prev_hash: prev_hash} =
          state
      ) do
    {:ok, requests} = MessageStore.select(@block_max_size, validator_id)
    {:ok, requests_df} = MessageStore.select_df(@block_max_size, validator_id)

    last_row_id = catch_last_row_id(requests)

    last_row_id_df = catch_last_row_id(requests_df)

    new_height =
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

      {:noreply, %{state | height: new_height}}
    else
      {:noreply, state}
    end
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  defp catch_last_row_id([]), do: -1

  defp catch_last_row_id(requests) do
    List.last(requests)
    |> List.last()
  end

  @doc """

  """
  def verify_block!(block, validator) do
    data_dir = Application.get_env(@otp_app, :data_dir)
    filename = "#{validator.id}.#{block.height}.#{@file_extension}"
    block_path = Path.join([data_dir, "blocks", filename])

    url = "https://#{validator.hostname}/v1/download/blocks/#{filename}"
    {:ok, _} = Download.from(url, path: block_path)

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

    decode_path = Path.join([data_dir, "blocks-decode", filename])

    :ok = File.write(decode_path, encode!(decode_events))

    decode_path
  end

  def mine_file(%{creator: creator_id, height: height, prev: prev_hash, round: round}) do
    data_dir = Application.get_env(@otp_app, :data_dir)
    block_path = Path.join([data_dir, "blocks-decode", "#{creator_id}.#{height}.#{@file_extension}"])
    {:ok, requests} = File.read(block_path)

    mine(requests, height, round, creator_id, prev_hash)
  end

  # def handle_block!(block, validator) do
  #   data_dir = Application.get_env(@otp_app, :data_dir)
  #   filename = "#{validator.id}.#{block.height}.#{@file_extension}"
  #   block_path = Path.join(data_dir, "blocks-decode/#{filename}")

  #   url = "https://#{validator.hostname}/v1/download/blocks-decode/#{filename}"
  #   Download.from(url, path: block_path)

  #   events =
  #     block_path
  #     |> File.read!()
  #     |> decode!()

  #   fun_valid!(validator.id, events)
  # end

  def fun_valid!(_, []), do: :ok

  def fun_valid!(validator_id, [{body, signature} | rest]) do
    hash = Blake3.hash(body)
    size = byte_size(body) + byte_size(signature)

    RequestHandler.valid!(hash, body, size, signature, validator_id)
    fun_valid!(validator_id, rest)
  end

  def fun_valid!(validator_id, [body | rest]) do
    hash = Blake3.hash(body)
    size = byte_size(body)

    RequestHandler.valid!(hash, body, size)
    fun_valid!(validator_id, rest)
  end

  # defp fun_reduce(
  #        [
  #          timestamp,
  #          hash,
  #          type,
  #          account_id,
  #          validator_id,
  #          args,
  #          message,
  #          signature,
  #          size,
  #          _rowid
  #        ],
  #        acc
  #      ) do
  #   try do
  #     args = decode_term(args)
  #     RequestHandler.handle!(hash, type, timestamp, account_id, validator_id, size, args)
  #     acc ++ [{message, signature}]
  #   rescue
  #     # block failed
  #     e ->
  #       Logger.debug(Exception.format(:error, e, __STACKTRACE__))
  #       acc
  #   end
  # end

  # defp fun_reduce(
  #        [
  #          _key,
  #          type,
  #          timestamp,
  #          hash,
  #          account_id,
  #          validator_id,
  #          args,
  #          message,
  #          signature,
  #          size,
  #          _rowid
  #        ],
  #        acc
  #      ) do
  #   try do
  #     args = decode_term(args)
  #     RequestHandler.handle!(hash, type, timestamp, account_id, validator_id, size, args)
  #     acc ++ [{message, signature}]
  #   rescue
  #     # block failed
  #     e ->
  #       Logger.debug(Exception.format(:error, e, __STACKTRACE__))
  #       acc
  #   end
  # end

  def mine(requests, height, round, validator_id, prev_hash) do
    data_dir = Application.get_env(@otp_app, :data_dir)

    block_path = Path.join([data_dir, "blocks", "#{height}.#{@file_extension}"])

    result =
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

    case result do
      [] ->
        Logger.debug("block empty #{height}")
        :ok = PubSub.broadcast(@pubsub_verifiers, "block", {"new", "empty"})
        :ok = PubSub.broadcast(@pubsub_network, "block", {"new", "empty"})
        height

      events ->
        Logger.debug(inspect(result))

        content = encode!(events)
        sync_all()

        :ok = File.write(block_path, content)
        block_size = File.stat!(block_path).size
        hashfile = hash_file(block_path)
        new_height = height + 1
        timestamp = :os.system_time(:millisecond)
        ev_count = length(events)

        block_map = %{
          height: new_height,
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
        privkey = Application.get_env(@otp_app, :privkey)
        {:ok, signature} = Cafezinho.Impl.sign(blockhash, privkey)
        block_map = Map.merge(block_map, %{hash: blockhash, signature: signature})

        Block.to_list(block_map)
        |> BlockStore.insert_sync()

        BlockStore.sync()

        :ok = PubSub.broadcast(@pubsub_verifiers, "block", {"new", block_map})
        :ok = PubSub.broadcast(@pubsub_network, "block", {"new", block_map})

        new_height
    end
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

  defp encode!(content) do
    :erlang.term_to_binary(content)
  end

  defp decode!(content) do
    :erlang.binary_to_term(content, [:safe])
  end

  @hash_module Blake3.Native
  defp hash_file(path) do
    state = @hash_module.new()

    File.stream!(path, [], 2048)
    |> Enum.reduce(state, &@hash_module.update(&2, &1))
    |> @hash_module.finalize()
  end

  defp decode_term(nil), do: nil

  defp decode_term(x) do
    :erlang.binary_to_term(x)
  end
end
