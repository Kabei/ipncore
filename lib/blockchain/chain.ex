defmodule Ipncore.Chain do
  require Logger
  use Ipnutils.FastGlobal, name: FG.Chain
  use GenServer
  alias Ipncore.{Block, Channel, Tx, Repo, Validator}

  @compile {:inline, get_time: 0, genesis_time: 0, prev_block: 0}

  @unit_time Default.unit_time()

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  @impl true
  def init(_opts) do
    {:ok, nil}
  end

  @impl true
  def handle_info({:start_block_round, channel}, last_key) do
    Logger.debug("start_block_round")

    genesis_time = genesis_time()
    iit = get_time()
    cycle = Default.block_interval()

    calc_time = cycle - rem(iit - genesis_time, cycle)

    Process.send_after(__MODULE__, {:start_block_round, channel}, calc_time)

    build_all(channel)

    {:noreply, last_key}
  end

  def get_time do
    # diff = get(:diff_iit, 0)
    # if diff == 0, do: throw(:error_time)
    :erlang.monotonic_time(@unit_time) + get(:diff_iit, 0)
  end

  def put_iit(iit_time) when iit_time > 0 do
    put(:diff_iit, iit_time - :erlang.monotonic_time(@unit_time))
  end

  def prev_block, do: get(:prev_block, nil)

  def put_prev_block(nil), do: :none

  def put_prev_block(%{time: time} = block) do
    put(:prev_time, time)
    put(:prev_block, Map.drop(block, [:txs]))
  end

  def prev_block_time, do: get(:prev_time, 0)

  def address, do: Application.get_env(:ipncore, :address)
  def address58, do: Application.get_env(:ipncore, :address58)

  def pool_address, do: Application.get_env(:ipncore, :pool)[:address] |> Base58Check.decode()

  def genesis_block, do: get(:genesis_block)
  def genesis_time, do: get(:genesis_time, 0)

  def put_genesis_block(block) do
    put(:genesis_block, block)
    put(:genesis_time, block.time)
  end

  def has_channel do
    get(:has_channel, false)
  end

  def initialize(channel_id) do
    Logger.info("Chain initialize")
    iit = get_time()

    case Channel.get(channel_id) do
      nil ->
        """
        No Channel
        IIT          #{Format.timestamp(iit)}
        """
        |> Logger.info()

      channel ->
        put(:has_channel, true)
        last_block = Block.fetch_last()
        put_prev_block(last_block)
        genesis_block = Block.fetch_genesis()

        if genesis_block do
          put(:genesis_block, genesis_block)
          put(:genesis_time, genesis_block.time)
        end

        send(__MODULE__, {:start_block_round, channel.id})

        if last_block do
          """
          Channel #{channel_id} Ready!
          IIT          #{Format.timestamp(iit)}
          Genesis Time #{Format.timestamp(genesis_block.time)}
          Last index   #{last_block.index}
          Total Blocks #{channel.block_count}
          """
        else
          """
          Channel Ready!
          IIT          #{Format.timestamp(iit)}
          No Blocks
          """
        end
        |> Logger.info()
    end
  end

  def add_block(prev_block, b, channel_id) do
    is_genesis_block = b.index == 0

    case BlockValidator.valid_block?(prev_block, b) do
      :ok ->
        Ecto.Multi.new()
        |> Ecto.Multi.insert(:block, Map.put(b, :txs, []), prefix: channel_id, returning: false)
        # |> Tx.set_status_complete(:txs, b.index, channel_id)
        # |> Tx.cancel_all_pending(:pending, b.index, channel_id)
        |> Channel.multi_put_genesis_time(:gen_time, channel_id, b.time, is_genesis_block)
        |> Channel.multi_update(:channel, channel_id, b.height, b.hash, b.amount, 1, b.ev_count)
        |> Repo.transaction()
        |> case do
          {:ok, _} ->
            # genesis block (after tx)
            if is_genesis_block do
              put_genesis_block(b)
            end

            put_prev_block(b)
            :ok

          err ->
            IO.inspect(err)

            Ecto.Multi.new()
            |> Tx.cancel_all_pending(:pending, b.index, channel_id)
            |> Repo.transaction()

            err
        end

      err ->
        IO.inspect(err)

        err
    end
  end

  # def build_next(channel) do
  #   if has_channel() do
  #     next_index = Block.next_index()
  #     next_index = if(next_index == 0, do: 0, else: next_index - 1)
  #     prev_block = prev_block()
  #     txs_approved = Tx.get_all_approved(next_index, channel)

  #     case Block.next(prev_block, txs_approved) do
  #       nil ->
  #         IO.inspect("nil block")
  #         nil

  #       block ->
  #         IO.inspect("adding block")
  #         add_block(block, prev_block, channel)
  #         block
  #     end
  #   else
  #     nil
  #   end
  # end

  def build_all(channel) do
    if has_channel() do
      before_next_index = next_index_to_build()

      if before_next_index >= 0 do
        txs_approved_grouped_by_block =
          Tx.get_all_approved(channel)
          |> Enum.filter(&(&1.block_index <= before_next_index))
          |> Enum.group_by(fn x -> x.block_index end)

        txs_approved_grouped_by_block
        |> Enum.map(fn {block_index, txs} ->
          prev_block = prev_block()
          block = Block.new(prev_block, block_index, txs)
          add_block(prev_block, block, channel)
          block
        end)
      else
        []
      end
    end
  end

  def next_index_to_build do
    index = Block.next_index()

    cond do
      index == 0 ->
        0

      true ->
        index - 1
    end
  end
end
