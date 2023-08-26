defmodule BlockTimer do
  use GenServer
  # alias Ippan.Validator
  alias Phoenix.PubSub
  require SqliteStore
  require Logger
  # alias Ippan.{Block, Round, Command, P2P}

  # import Ippan.Block,
  #   only: [decode_file!: 1, encode_file!: 1, hash_file: 1, put_hash: 1, put_signature: 1]

  @otp_app :ipncore
  @module __MODULE__
  # @token Application.compile_env(:ipncore, :token)
  @pubsub_verifiers :cluster
  # @topic_block "block"
  @topic_round "round"
  # @topic_jackpot "jackpot"
  @block_interval Application.compile_env(@otp_app, :block_interval)
  # @max_block_size Application.compile_env(@otp_app, :max_block_size)
  # @block_data_max_size Application.compile_env(@otp_app, :block_data_max_size)
  # @blockchain_version Application.compile_env(@otp_app, :version)

  def start_link(opts) do
    GenServer.start_link(@module, opts, name: @module, hibernate_after: 5_000)
  end

  def get_last_block(conn, stmts, validator_id) do
    case SqliteStore.step(conn, stmts, "last_block", [validator_id]) do
      {:row, [id, _creator, hash | _rest]} ->
        {id, hash}

      _ ->
        {0, nil}
    end
  end

  def get_last_round(conn, stmts) do
    case SqliteStore.step(conn, stmts, "last_round", []) do
      {:row, [round_id, hash | _]} -> {round_id, hash}
      _ -> {0, nil}
    end
  end

  @impl true
  def init(_args) do
    %{vid: validator_id} = Platform.start()
    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmts)

    {block_id, block_hash} = get_last_block(conn, stmts, validator_id)

    {round_id, round_hash} = get_last_round(conn, stmts)

    total_validators = SqliteStore.total(conn, stmts, "total_validator")

    my_last_candidate = :persistent_term.get(:candidate)

    # SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", , Validator)
    cpus = System.schedulers_online()

    {:ok, pool_server} =
      :poolboy.start_link(
        worker_module: MinerWorker,
        size: trunc(cpus * 0.6),
        max_overflow: trunc(cpus * 0.2)
      )

    {:ok,
     %{
       state: :sync,
       sync: false,
       block: block_id,
       round: round_id,
       prev_block: block_hash,
       prev_round: round_hash,
       validators: total_validators,
       candidate: my_last_candidate,
       pool: pool_server,
       mined: 0,
       wait_to_sync: false
     }}
  end

  def who_is_on_duty(round, total_validators) do
    rem(round, total_validators)
  end

  # @impl true
  # def handle_continue(
  #       {:continue, _block_round},
  #       %{
  #         mined: mined,
  #         next_round: next_round,
  #         next_block: next_block,
  #         validators: validators
  #       } = state
  #     ) do
  #   cond do
  #     next_block > next_round ->
  #       if validators == mined do
  #         send(self(), :sync)
  #         {:noreply, %{state | block_sync: true}}
  #       else
  #         # {:ok, tRef} = :timer.send_after(@block_interval, :mine)
  #         {:noreply, %{state | block_sync: true}}
  #       end

  #     true ->
  #       {:ok, tRef} = :timer.send_after(@block_interval, :mine)
  #       {:noreply, %{state | tRef: tRef}}
  #   end
  # end

  # build a round
  def sync do
    GenServer.cast(@module, :sync)
  end

  def put_validators(n) do
    GenServer.cast(@module, {:validators, n})
  end

  def get_state do
    GenServer.call(BlockTimer, :state)
  end

  @impl true
  def handle_cast({:complete, :block, _block}, %{mined: mined} = state) do
    if EnvStore.blocks_per_round() == mined do
      send(self(), :sync)
      {:noreply, %{state | mined: mined + 1}}
    else
      {:noreply, %{state | mined: mined + 1}}
    end
  end

  def handle_cast({:complete, :round, new_id, old_id, new_hash, _blocks}, state) do
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

  @impl true
  def handle_call(:round, _from, %{next_round: id} = state) do
    {:reply, id, state}
  end

  def handle_call(:height, _from, %{next_block: id} = state) do
    {:reply, id, state}
  end

  def handle_call(
        :state,
        _from,
        %{next_round: next_round, next_block: next_block, prev_round: prev_round, status: status} =
          state
      ) do
    {:reply, %{round: next_round - 1, hash: prev_round, height: next_block - 1, status: status},
     state}
  end

  @impl true
  def handle_info(
        :sync,
        %{
          sync: false,
          validators: _validators,
          mined: _mined
        } = state
      ) do
    # IO.inspect("sync")
    # IO.inspect(state)

    # spawn_round_worker(self(), state)
    {:noreply, %{state | sync: true}}
  end

  def handle_info(:sync, state) do
    # IO.inspect("sync ELSE")
    # IO.inspect(state)
    {:noreply, %{state | wait_to_sync: true}}
  end

  def handle_info({:candidate, block}, state) do
    {:noreply, %{state | candidate: block}}
  end

  @impl true
  def terminate(_reason, %{pool: pool_server}) do
    :poolboy.stop(pool_server)
    # PubSub.unsubscribe(@pubsub_verifiers, "event")
  end

  # defp decode_term(nil), do: nil

  # defp decode_term(x) do
  #   :erlang.binary_to_term(x)
  # end
end
