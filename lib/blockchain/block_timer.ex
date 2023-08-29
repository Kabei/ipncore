defmodule BlockTimer do
  use GenServer
  alias Ippan.NetworkNode
  alias Ippan.Validator
  alias Phoenix.PubSub
  alias Ippan.Block
  require SqliteStore
  require Logger
  # alias Ippan.Validator
  # alias Ippan.{Block, Round, Command, P2P}

  # import Ippan.Block,
  #   only: [decode_file!: 1, encode_file!: 1, hash_file: 1, put_hash: 1, put_signature: 1]

  @otp_app :ipncore
  @module __MODULE__
  # @token Application.compile_env(:ipncore, :token)
  @pubsub :cluster
  @block_interval Application.compile_env(@otp_app, :block_interval)
  # @topic_block "block"
  # @topic_round "round"
  # @topic_jackpot "jackpot"
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

  def who_is_on_duty(round, total_validators) do
    rem(round, total_validators)
  end

  @max_peers_conn Application.compile_env(:ipncore, :max_peers_conn)

  def connect_to_peers(validators) do
    take = max(@max_peers_conn - NetworkNode.count(), 0)

    if take > 0 do
      (validators -- NetworkNode.list())
      |> Enum.take_random(take)
      |> Enum.each(fn x ->
        NetworkNode.connect(x, retry: 3)
      end)
    end
  end

  @impl true
  def init(_args) do
    %{vid: validator_id} = Platform.start()

    conn = :persistent_term.get(:asset_conn)
    stmts = :persistent_term.get(:asset_stmt)

    ets_nodes = :ets.new(:nodes, [:set, :public])

    validators = SqliteStore.all(conn, stmts, "get_players")

    # fill nodes table
    for v <- validators do
      :ets.insert(ets_nodes, Validator.to_tuple(v))
    end

    # connect to P2P peers
    connect_to_peers(validators)



    {block_id, block_hash} = get_last_block(conn, stmts, validator_id)

    {round_id, round_hash} = get_last_round(conn, stmts)

    total_validators = SqliteStore.total(conn, stmts, "total_validator")

    my_candidates = DetsPlus.lookup(:g, "candidates")

    {:ok, dets} =
      DetsPlus.open_file(:g,
        file: Path.join(:persistent_term.get(:store_dir), "priv/g.dets"),
        auto_save: 30_000
      )

    # SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", , Validator)
    cpus = System.schedulers_online()

    {:ok, pool_server} =
      :poolboy.start_link(
        worker_module: MinerWorker,
        size: ceil(cpus * 0.6),
        max_overflow: trunc(cpus * 0.2)
      )

    :timer.send_after(@block_interval, :tick)

    {:ok,
     %{
       state: :sync,
       sync: false,
       dets: dets,
       block: block_id,
       round: round_id,
       prev_block: block_hash,
       prev_round: round_hash,
       validators: total_validators,
       candidates: my_candidates,
       pool: pool_server,
       mined: 0,
       wait_to_sync: false
     }}
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

  def new_candidate(index, block) do
    DetsPlus.map_put(:g, "candidates", index, block)
  end

  def get_state do
    GenServer.call(BlockTimer, :state)
  end

  @impl true
  def handle_cast(
        {:msg_round,
         %{id: id, creator: creator_id, blocks: blocks, time: time, signature: signature} = round,
         _from},
        %{
          ets_votes: ets_votes,
          main: {conn, stmts},
          last: last_round,
          min_votes: min_votes,
          total: total_valdiators
        } = state
      ) do
    validator =
      SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", creator_id, Validator)

    cond do
      last_round.time > time ->
        :error

      who_is_on_duty(id, total_valdiators) != creator_id ->
        :error

      EnvStore.blocks_per_round() <= length(blocks) ->
        :error

      Cafezinho.Impl.verify(signature, signature, validator.pubkey) != :ok ->
        :error

      true ->
        if :ets.update_counter(ets_votes, round, {2, 1}, {round, 1}) >= min_votes do
          if last_round.id + 1 == id do
            # send to build blocks and round

            [round] = :ets.lookup(ets_votes, round)
          end
        else
          :ok
        end
    end

    {:noreply, state}
  end

  def handle_cast(
        {:msg_block,
         %{
           hash: hash,
           prev: _prev,
           height: height,
           creator: creator_id,
           count: _count,
           size: _size,
           signature: signature,
           hashfile: _hashfile,
           timestamp: _timestamp
         } = block},
        _from,
        %{ets_blocks: ets_blocks, main: {conn, stmts}} = state
      ) do
    validator =
      SqliteStore.lookup_map(:validator, conn, stmts, "get_validator", creator_id, Validator)

    cond do
      SqliteStore.exists?(conn, stmts, "get_block", [creator_id, height]) ->
        :error

      Cafezinho.Impl.verify(signature, hash, validator.pubkey) != :ok ->
        :error

      true ->
        :ets.insert(ets_blocks, {block})
    end

    {:noreply, state}
  end

  def handle_cast({:complete, :block, _block}, %{mined: mined, round: round} = state) do
    if round == mined do
      send(self(), :sync)
      {:noreply, %{state | mined: mined + 1}}
    else
      {:noreply, %{state | mined: mined + 1}}
    end
  end

  def handle_cast({:complete, :round, new_round, _old_round, _blocks}, state) do
    # IO.inspect("complete round")
    # IO.inspect(state)
    # VoteCounter.reset(old_round)
    # send pubsub event
    PubSub.local_broadcast(@pubsub, "cluster", %{"event" => "round.new", "data" => new_round})

    if state.wait_sync do
      send(self(), :sync)
    end

    {:noreply,
     %{
       state
       | round: new_round.id,
         last: new_round,
         sync: false,
         wait_sync: false,
         mined: 0
     }}
  end

  def handle_cast({:validators, n}, %{validators: validators} = state) do
    {:noreply, %{state | total: validators + n}}
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
  def handle_info(:tick, state) do
    :timer.send_after(@block_interval, :tick)
    {:noreply, state}
  end

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
  end
end
