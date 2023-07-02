defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, RequestHandler}
  require Logger

  @otp_app :ipncore
  @pubsub_verifiers :verifiers
  @pubsub_network :network
  @block_interval Application.compile_env(:ipncore, :block_interval)
  @block_max_size Application.compile_env(:ipncore, :block_max_size)
  @block_version Application.compile_env(:ipncore, :block_version)
  @file_extension "erl"

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @impl true
  def init(_args) do
    data_dir = Application.get_env(@otp_app, :data_dir, "data")
    block_dir = Path.join(data_dir, "blocks")
    block_decode_dir = Path.join(data_dir, "blocks-decode")
    File.mkdir(block_dir)
    File.mkdir(block_decode_dir)

    validator_id = Default.validator_id()

    case BlockStore.last(validator_id) do
      {:row, block_list} ->
        block = Block.to_map(block_list)

        {:ok,
         %{
           height: block.height,
           round: block.round,
           validator_id: validator_id
         }}

      _ ->
        {:ok,
         %{
           height: 0,
           round: 0,
           validator_id: validator_id
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
    time = :os.system_time()

    for [_key, type, timestamp, hash, account_id, validator_id, args, _message, _signature, size] <-
          requests do
      RequestHandler.handle_post!(
        hash,
        type,
        timestamp,
        account_id,
        validator_id,
        size,
        decode_term(args)
      )
    end

    {:ok, [[blocks]]} = BlockStore.count_round(round)
    RoundStore.insert([round, blocks, time])

    sync_all()
    MessageStore.sync()
    RoundStore.sync()

    {:reply, round, %{state | round: round + 1}}
  end

  @impl true
  def handle_cast(:start, state) do
    tref = :timer.send_after(@block_interval, :mine)
    {:noreply, Map.put(state, :tref, tref)}
  end

  @impl true
  def handle_info(:mine, %{height: height, round: old_round, validator_id: validator_id} = state) do
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
        last_row_id,
        last_row_id_df
      )

    {:noreply, %{state | height: new_height}}
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
    data_dir = Application.get_env(@otp_app, :data_dir, "data")
    filename = "#{validator.id}.#{block.height}.#{@file_extension}"
    block_path = Path.join([data_dir, "blocks", filename])

    url = "https://#{validator.hostname}/download/blocks/#{filename}"
    {:ok, _} = Download.from(url, path: block_path)

    {:ok, content} = File.read(block_path)

    block_hash = hash_file(block_path)

    if block.hash != block_hash, do: raise(IppanError, "Hash block file is invalid")

    events = decode!(content)

    decode_events =
      for {body, signature} <- events do
        hash = Blake3.hash(body)
        size = byte_size(body) + byte_size(signature)

        RequestHandler.valid!(hash, body, size, signature, validator.id)
      end

    decode_path = Path.join([data_dir, "blocks-decode", filename])

    File.write(decode_path, encode!(decode_events))
  end

  @doc """

  """
  def handle_block!(block, validator) do
    data_dir = Application.get_env(@otp_app, :data_dir, "data")
    filename = "#{validator.id}.#{block.height}.#{@file_extension}"
    block_path = Path.join(data_dir, "blocks-decode/#{filename}")

    url = "https://#{validator.hostname}/download/blocks-decode/#{filename}"
    Download.from(url, path: block_path)

    events =
      block_path
      |> File.read!()
      |> decode!()

    fun_valid!(validator.id, events)
  end

  defp fun_valid!(_, []), do: :ok

  defp fun_valid!(validator_id, [{body, signature} | rest]) do
    hash = Blake3.hash(body)
    size = byte_size(body) + byte_size(signature)

    RequestHandler.valid!(hash, body, size, signature, validator_id)
    fun_valid!(validator_id, rest)
  end

  defp fun_valid!(validator_id, [body | rest]) do
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

  defp mine(requests, height, round, validator_id, last_row_id, last_row_id_def) do
    data_dir = Application.get_env(@otp_app, :data_dir, "data")

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

        block_list_format = [
          new_height,
          validator_id,
          hashfile,
          round,
          timestamp,
          ev_count,
          block_size,
          @block_version
        ]

        BlockStore.insert(block_list_format)

        block_map = %{
          height: new_height,
          creator: validator_id,
          hashfile: hashfile,
          round: round,
          timestamp: timestamp,
          ev_count: ev_count,
          size: block_size,
          vsn: @block_version
        }

        MessageStore.delete_all(last_row_id)
        MessageStore.delete_all_df(last_row_id_def)

        MessageStore.sync()
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
