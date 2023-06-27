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
    File.mkdir(block_dir)

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

  def start do
    GenServer.cast(BlockTimer, :init)
  end

  def get_round do
    GenServer.call(BlockTimer, :round, :infinity)
  end

  @impl true
  def handle_call(:round, _from, %{round: round} = state) do
    {:reply, round, state}
  end

  def handle_call(:next_round, _from, %{round: round} = state) do
    {:reply, round, %{state | round: round + 1}}
  end

  @impl true
  def handle_cast(:start, state) do
    tref = :timer.send_after(@block_interval, :mine)
    {:noreply, Map.put(state, :tref, tref)}
  end

  @impl true
  def handle_info(:mine, %{height: height, round: old_round, validator_id: validator_id} = state) do
    {:ok, requests} = MessageStore.select(@block_max_size)
    {:ok, requests_df} = MessageStore.select_df(@block_max_size)

    last_row_id =
      List.last(requests)
      |> List.last()

    last_row_id_df =
      List.last(requests_df)
      |> List.last()

    spawn_link(fn ->
      mine(requests ++ requests_df, height, old_round, validator_id, last_row_id, last_row_id_df)
    end)

    {:noreply, state}
  end

  @impl true
  def terminate(_reason, _state) do
    PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  defp mine([], _height, _old_round, _validator_id) do
    Logger.debug("block empty")
    :ok = PubSub.broadcast(@pubsub_verifiers, "block", {"new", "empty"})
    :ok = PubSub.broadcast(@pubsub_network, "block", {"new", "empty"})
  end

  defp mine(requests, height, old_round, validator_id, last_row_id, last_row_id_def) do
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
            RequestHandler.handle!(hash, type, timestamp, account_id, validator_id, size, args)
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
          size
        ],
        acc ->
          try do
            args = decode_term(args)
            RequestHandler.handle!(hash, type, timestamp, account_id, validator_id, size, args)
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
        mine([], height, old_round, validator_id)

      events ->
        Logger.debug(inspect(result))

        content = encode!(events)

        sync_all()

        :ok = File.write(block_path, content)
        block_size = File.stat!(block_path).size
        hashfile = hash_file(block_path)
        new_height = height + 1
        new_round = old_round + 1
        timestamp = :os.system_time(:millisecond)
        ev_count = length(events)

        block_list_format = [
          new_height,
          validator_id,
          hashfile,
          new_round,
          timestamp,
          ev_count,
          block_size,
          @block_version
        ]

        BlockStore.insert(block_list_format)

        block_map = Block.to_map(block_list_format)

        MessageStore.delete_all(last_row_id)
        MessageStore.delete_all_df(last_row_id_def)

        BlockStore.sync()
        MessageStore.sync()

        :ok = PubSub.broadcast(@pubsub_verifiers, "block", {"new", block_map})
        :ok = PubSub.broadcast(@pubsub_network, "block", {"new", block_map})
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
