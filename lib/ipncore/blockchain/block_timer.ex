defmodule BlockTimer do
  use GenServer
  alias Phoenix.PubSub
  alias Ippan.{Block, Round, RequestHandler, P2P}

  import Ippan.Block,
    only: [decode_file!: 1, encode_file!: 1, hash_file: 1, put_hash: 1, put_signature: 1]

  require Logger
  require Global

  @otp_app :ipncore
  @module __MODULE__
  @token Application.compile_env(:ipncore, :token)
  @pubsub_verifiers :verifiers
  @topic_block "block"
  @topic_round "round"
  @block_interval Application.compile_env(@otp_app, :block_interval)
  @block_max_size Application.compile_env(@otp_app, :block_max_size)
  @block_data_max_size Application.compile_env(@otp_app, :block_data_max_size)
  @blockchain_version Application.compile_env(@otp_app, :version)

  def start_link(opts) do
    GenServer.start_link(@module, opts, name: @module, hibernate_after: 5_000)
  end

  @impl true
  def init(_args) do
    validator_id = Global.validator_id()

    {:ok, pool_server} =
      :poolboy.start_link(
        worker_module: MinerWorker,
        size: 5,
        max_overflow: 0
      )

    # fetch last round
    {round_id, round_hash} =
      case RoundStore.last() do
        {:row, [round_id, hash | _]} -> {round_id + 1, hash}
        _ -> {0, nil}
      end

    # fetch last block
    {block_id, block_hash, block_round} =
      case BlockStore.last(validator_id) do
        {:row, [id, _creator, hash, _prev, _hashfile, _sig, block_round | _rest]} ->
          {id + 1, hash, block_round}

        _ ->
          {0, nil, 0}
      end

    block_unique_ids = BlockStore.fetch_uniques(round_id)

    {:ok,
     %{
       block_sync: false,
       blocks: block_unique_ids,
       mined: length(block_unique_ids),
       next_block: block_id,
       next_round: round_id,
       prev_block: block_hash,
       prev_round: round_hash,
       round_sync: false,
       tRef: nil,
       validators: ValidatorStore.total(),
       validator_id: validator_id,
       wait_to_mine: false,
       wait_to_sync: false,
       pool: pool_server
     }, {:continue, {:continue, block_round}}}
  end

  @impl true
  def handle_continue(
        {:continue, _block_round},
        %{
          mined: mined,
          next_round: next_round,
          next_block: next_block,
          validators: validators
        } = state
      ) do
    cond do
      next_block > next_round ->
        if validators == mined do
          send(self(), :sync)
          {:noreply, %{state | block_sync: true}}
        else
          # {:ok, tRef} = :timer.send_after(@block_interval, :mine)
          {:noreply, %{state | block_sync: true}}
        end

      true ->
        {:ok, tRef} = :timer.send_after(@block_interval, :mine)
        {:noreply, %{state | tRef: tRef}}
    end
  end

  # build a round
  def sync do
    GenServer.cast(@module, :sync)
  end

  def put_validators(n) do
    GenServer.cast(@module, {:validators, n})
  end

  def put_block_ignore_mine do
    GenServer.cast(@module, "ignore_mine")
  end

  def check_state(state) do
    GenServer.call(BlockTimer, {:check_state, state})
  end

  @impl true
  def handle_cast(
        {:complete, :block, new_height, creator_id, new_hash},
        %{
          mined: mined,
          validator_id: validator_id,
          validators: validators
        } = state
      ) do
    # IO.inspect("complete block")
    # IO.inspect(state)
    mined = mined + 1

    return =
      if validator_id == creator_id do
        {:noreply,
         %{
           state
           | next_block: new_height,
             prev_block: new_hash,
             mined: mined,
             wait_to_mine: false,
             wait_to_sync: false
         }}
      else
        {:noreply, %{state | mined: mined + 1, block_sync: false, wait_to_sync: false}}
      end

    if mined == validators do
      send(self(), :sync)
    end

    return
  end

  # complete mine foreign block
  def handle_cast(
        {:complete, :import, _block},
        %{
          mined: mined,
          validators: validators,
          round_sync: round_sync
        } = state
      ) do
    # IO.inspect("complete block import")
    # IO.inspect(state)
    mined = mined + 1

    if not round_sync and mined == validators do
      send(BlockTimer, :sync)
    end

    {:noreply, %{state | mined: mined}}
  end

  def handle_cast({:complete, :round, new_id, old_id, new_hash}, state) do
    # IO.inspect("complete round")
    # IO.inspect(state)
    VoteCounter.reset(old_id)
    # send pubsub event
    PubSub.broadcast(@pubsub_verifiers, @topic_round, {"end", old_id})
    PubSub.broadcast(@pubsub_verifiers, @topic_round, {"start", new_id})

    tRef =
      if state.wait_to_mine do
        send(self(), :mine)
      else
        {:ok, tRef} = :timer.send_after(@block_interval, :mine)
        tRef
      end

    {:noreply,
     %{
       state
       | block_sync: false,
         blocks: [],
         next_round: new_id,
         prev_round: new_hash,
         round_sync: false,
         mined: 0,
         tRef: tRef
     }}
  end

  def handle_cast({:validators, n}, %{validators: validators} = state) do
    {:noreply, %{state | validators: validators + n}}
  end

  def handle_cast("ignore_mine", %{mined: mined} = state) do
    {:noreply, %{state | mined: mined + 1}}
  end

  @impl true
  def handle_call(:round, _from, %{next_round: id} = state) do
    {:reply, id, state}
  end

  def handle_call(:height, _from, %{next_block: id} = state) do
    {:reply, id, state}
  end

  def handle_call(:state, _from, state) do
    {:reply, state, state}
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

  # mine foreign block
  def handle_info(
        {:import, %{creator: creator_id, height: height, round: round} = block},
        %{
          blocks: blocks,
          next_round: next_round,
          validator_id: me,
          pool: pool_server
        } = state
      )
      when creator_id != me do
    pid = self()
    unique_block_id = {creator_id, height}

    if round == next_round and unique_block_id not in blocks do
      spawn(fn ->
        :poolboy.transaction(
          pool_server,
          fn worker_pid ->
            GenServer.cast(worker_pid, {:remote, pid, block})
          end,
          :infinity
        )
      end)

      {:noreply, %{state | blocks: :lists.append(blocks, [unique_block_id])}}
    else
      {:noreply, state}
    end
  end

  def handle_info(
        :sync,
        %{
          round_sync: false,
          validators: validators,
          mined: mined,
          tRef: tRef
        } = state
      )
      when mined == validators do
    # IO.inspect("sync")
    # IO.inspect(state)
    :timer.cancel(tRef)
    spawn_round_worker(self(), state)
    {:noreply, %{state | round_sync: true}}
  end

  def handle_info(:sync, state) do
    # IO.inspect("sync ELSE")
    # IO.inspect(state)
    {:noreply, %{state | wait_to_sync: true}}
  end

  @impl true
  def terminate(reason, %{tRef: tRef, pool: pool_server}) do
    :timer.cancel(tRef)
    GenServer.stop(pool_server, reason)
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

      new_block =
        mine_fun(
          requests ++ requests_df,
          next_block,
          next_round,
          validator_id,
          prev_block,
          :os.system_time(:millisecond),
          :local
        )

      P2P.push(["new_recv", new_block])

      MessageStore.sync()
      MessageStore.delete_all(last_row_id)
      MessageStore.delete_all_df(last_row_id_df)

      GenServer.cast(pid, {:complete, :block, next_block + 1, validator_id, new_block.hash})
    end)
  end

  defp spawn_round_worker(pid, %{next_round: current_round, prev_round: prev_round}) do
    spawn_link(fn ->
      {:ok, requests} = MessageStore.select_all_df_approved(current_round)
      MessageStore.delete_all_df_approved(current_round)

      Enum.each(requests, fn [
                               hash,
                               timestamp,
                               _key,
                               type,
                               account_id,
                               validator_id,
                               node_id,
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
          node_id,
          size,
          decode_term(args)
        )
      end)

      result_hashes = BlockStore.fetch_hash_round(current_round)
      hashes = Enum.concat(result_hashes)
      count = length(hashes)
      hash = Round.compute_hash(current_round, prev_round, hashes)
      {:row, [timestamp]} = BlockStore.avg_round_time(current_round)

      RoundStore.insert([current_round, hash, prev_round, count, timestamp, @blockchain_version])

      task_jackpot =
        Task.async(fn ->
          run_jackpot(current_round, hash, timestamp)
          MessageStore.delete_expiry(current_round, timestamp)
          DomainStore.delete_expiry(current_round, timestamp)
        end)

      commit()

      Task.await(task_jackpot, :infinity)
      checkpoint_commit()

      GenServer.cast(pid, {:complete, :round, current_round + 1, current_round, hash})
    end)
  end

  # Create a block file and register transactions
  def mine_fun(requests, height, round, creator_id, prev_hash, block_timestamp, format) do
    block_path = Block.block_path(creator_id, height)

    {events, error} =
      case format do
        :local ->
          Enum.reduce_while(requests, %{}, fn
            [
              hash,
              timestamp,
              type,
              account_id,
              validator_id,
              node_id,
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
                  node_id,
                  size,
                  args,
                  round
                )

                {:cont, Map.put_new(acc, hash, [message, signature])}
              rescue
                # block failed
                e ->
                  Logger.debug(Exception.format(:error, e, __STACKTRACE__))
                  {:halt, {acc, true}}
              end

            [
              hash,
              timestamp,
              _key,
              type,
              account_id,
              validator_id,
              node_id,
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
                  node_id,
                  size,
                  args,
                  round
                )

                {:cont,
                 Map.put_new(acc, hash, if(signature, do: [message, signature], else: message))}
              rescue
                # block failed
                e ->
                  Logger.debug(Exception.format(:error, e, __STACKTRACE__))
                  {:cont, acc}
              end
          end)

        _import ->
          Enum.reduce_while(requests, %{}, fn
            [
              hash,
              timestamp,
              type,
              account_id,
              validator_id,
              node_id,
              args,
              message,
              signature,
              size
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
                  node_id,
                  size,
                  args,
                  round
                )

                {:cont, Map.put_new(acc, hash, [message, signature])}
              rescue
                # tx failed
                e ->
                  Logger.debug(Exception.format(:error, e, __STACKTRACE__))
                  {:halt, {acc, true}}
              end

            [
              hash,
              timestamp,
              _key,
              type,
              account_id,
              validator_id,
              node_id,
              args,
              message,
              signature,
              size
            ] = msg,
            acc ->
              try do
                case MessageStore.insert_df(msg) do
                  :done ->
                    args = decode_term(args)

                    RequestHandler.handle!(
                      hash,
                      type,
                      timestamp,
                      account_id,
                      validator_id,
                      node_id,
                      size,
                      args,
                      round
                    )

                    {:cont,
                     Map.put_new(
                       acc,
                       hash,
                       if(signature, do: [message, signature], else: message)
                     )}

                  _ ->
                    {:cont, acc}
                end
              rescue
                # tx failed
                e ->
                  MessageStore.delete_df(hash)
                  Logger.debug(Exception.format(:error, e, __STACKTRACE__))
                  {:cont, acc}
              end
          end)
      end
      |> case do
        {events, true} -> {:maps.values(events), true}
        events -> {:maps.values(events), false}
      end

    ev_count = length(events)
    empty = ev_count == 0

    {hashfile, block_size} =
      if empty do
        {nil, 0}
      else
        content = encode_file!(events)
        :ok = File.write(block_path, content)
        hashfile = hash_file(block_path)
        block_size = File.stat!(block_path).size
        commit()
        {hashfile, block_size}
      end

    block =
      %{
        height: height,
        prev: prev_hash,
        creator: creator_id,
        hashfile: hashfile,
        round: round,
        timestamp: block_timestamp,
        ev_count: ev_count,
        size: block_size,
        error: error
      }
      |> put_hash()
      |> put_signature()

    # IO.inspect(block)

    Block.to_list(block)
    |> BlockStore.insert_sync()

    # BlockStore.sync()

    Logger.debug(
      "Block #{creator_id}.#{height} | events: #{ev_count} | hash: #{Base.encode16(block.hash)}"
    )

    PubSub.broadcast(@pubsub_verifiers, @topic_block, {"new", block})

    block
  end

  @doc """
  Verify block metadata only
  """
  @spec verify(term, binary) :: :ok | :empty | :error
  def verify(
        %{
          creator: creator,
          hash: hash,
          height: height,
          ev_count: 0,
          hashfile: hashfile,
          prev: prev,
          round: round,
          size: size,
          signature: signature,
          timestamp: timestamp
        } = _block,
        creator_pubkey
      ) do
    cond do
      hashfile != nil ->
        :error

      0 != size ->
        :error

      Block.compute_hash(height, creator, round, prev, hashfile, timestamp) != hash ->
        :error

      Cafezinho.Impl.verify(signature, hash, creator_pubkey) != :ok ->
        :error

      true ->
        :empty
    end
  end

  def verify(
        %{
          creator: creator,
          round: round,
          prev: prev,
          hash: hash,
          height: height,
          hashfile: hashfile,
          signature: signature,
          timestamp: timestamp
        } = _block,
        creator_pubkey
      ) do
    cond do
      Cafezinho.Impl.verify(signature, hash, creator_pubkey) != :ok ->
        :error

      Block.compute_hash(height, creator, round, prev, hashfile, timestamp) != hash ->
        :error

      true ->
        :ok
    end
  end

  # Verify a block file and conver to decode block file
  @spec verify_file!(term, term) :: :ok
  def verify_file!(
        %{
          height: height,
          hash: hash,
          round: round,
          hashfile: hashfile,
          creator: creator_id,
          prev: prev,
          signature: signature,
          timestamp: timestamp,
          ev_count: ev_count,
          size: size
        } = block,
        %{hostname: hostname, pubkey: pubkey}
      ) do
    remote_url = Block.url(hostname, creator_id, block.height)
    output_path = Block.block_path(creator_id, block.height)
    file_exists = File.exists?(output_path)
    filename = Path.basename(output_path)

    unless file_exists do
      :ok = Curl.download_block(remote_url, output_path)
    else
      {:ok, filestat} = File.stat(output_path)

      if filestat.size != size do
        :ok = Curl.download_block(remote_url, output_path)
      end
    end

    {:ok, filestat} = File.stat(output_path)

    if filestat.size > @block_max_size or filestat.size != size do
      raise IppanError, "Invalid block size"
    end

    if hash != Block.compute_hash(height, creator_id, round, prev, hashfile, timestamp) do
      raise(IppanError, "Invalid block hash")
    end

    if hashfile != hash_file(output_path) do
      raise(IppanError, "Hash block file is invalid")
    end

    if Cafezinho.Impl.verify(signature, hash, pubkey) != :ok do
      raise(IppanError, "Invalid block signature")
    end

    {:ok, content} = File.read(output_path)
    events = decode_file!(content)

    decode_events =
      Enum.reduce(
        %{},
        events,
        fn
          [body, signature], acc ->
            hash = Blake3.hash(body)
            size = byte_size(body) + byte_size(signature)

            msg =
              RequestHandler.valid!(hash, body, size, signature, creator_id)
              |> elem(1)

            Map.put_new(acc, hash, msg)

          body, acc ->
            hash = Blake3.hash(body)
            size = byte_size(body)

            msg =
              RequestHandler.valid!(hash, body, size)
              |> elem(1)

            Map.put_new(acc, hash, msg)
        end
      )
      |> Map.values()

    if ev_count != length(decode_events) do
      raise(IppanError, "Invalid block size and valid events size")
    end

    export_path =
      Application.get_env(@otp_app, :decode_dir)
      |> Path.join(filename)

    :ok = File.write(export_path, encode_file!(decode_events))
  end

  # Pay a player according to the calculation of the remaining
  # hash of the round ordered by activity in the last 24 hours
  defp run_jackpot(round_id, hash, round_timestamp) do
    # time_activity = round_timestamp - @time_activity

    num =
      :binary.part(hash, 24, 8)
      |> :binary.decode_unsigned()

    count = WalletStore.total()

    if count > 0 do
      position = rem(num, count) + 1
      winner_id = WalletStore.jackpot(position)
      amount = EnvStore.jackpot_reward()
      RoundStore.insert_winner(round_id, winner_id, amount)
      TokenStore.sum_suppy(@token, amount)
      BalanceStore.income(winner_id, @token, amount, round_timestamp)
      BalanceStore.sync()
      Logger.debug("[Jackpot] Winner: #{winner_id} | #{amount}")
      :ok
    else
      Logger.debug("[Jackpot] No winner")
      :none
    end
  end

  defp commit do
    Logger.debug("commit")
    MessageStore.sync()
    BlockStore.sync()
    RoundStore.sync()
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
