defmodule Ipncore.Chain do
  require Logger
  # use GenServer
  alias Ipncore.{Block, IIT, Repo, BlockValidator}

  @compile {:inline, get_time: 0, genesis_time: 0, last_block: 0, events: 0}
  @base :chain
  @table :chain
  @filename "chain.db"
  @unit_time Default.unit_time()

  defmacrop put(atom_name, val) do
    quote do
      obj = {unquote(atom_name), unquote(val)}
      :ets.insert(@table, obj)
      DetsPlus.insert(@base, obj)
    end
  end

  defmacrop get(atom_name, default \\ nil) do
    quote do
      case :ets.lookup(@table, unquote(atom_name)) do
        [{_k, val}] -> val
        _ -> unquote(default)
      end
    end
  end

  def open do
    :ets.whereis(@table)
    |> case do
      :undefined ->
        :ets.new(@table, [
          :set,
          :public,
          :named_table,
          read_concurrency: true,
          write_concurrency: true
        ])

      _ ->
        :ok
    end

    dir_path = Default.data_dir()
    filename = Path.join(dir_path, @filename)
    r = DetsPlus.open_file(@base, file: filename, auto_save: 60_000)

    # load from DB to ets
    DetsPlus.reduce(@base, nil, fn x, _acc ->
      :ets.insert(@table, x)
    end)

    r
  end

  def close do
    DetsPlus.close(@base)
  end

  def sync do
    DetsPlus.sync(@base)
  end

  def last_block do
    get(:last_block)
  end

  def get_time do
    # diff = get(:diff_iit, 0)
    # if diff == 0, do: throw(:error_time)
    :os.system_time(@unit_time) + get(:diff_time, 0)
  end

  def next_index do
    case get(:next_index) do
      nil -> 0
      n -> n
    end
  end

  # @impl true
  # def handle_call(:diff_time, _from, state) do
  #   {:reply, Map.get(state, :diff_iit), state}
  # end

  # def handle_call(:last_block, _from, state) do
  #   {:reply, Map.get(state, :last_block), state}
  # end

  # def handle_call(:height, _from, state) do
  #   {:reply, Map.get(state, :height), state}
  # end

  # @impl true
  # def handle_cast({:last_block, block}, state) do
  #   new_state = Map.merge(state, %{last_block: block, height: block.height})
  #   {:noreply, new_state}
  # end

  def put_iit(iit_time) when iit_time > 0 do
    put(:diff_iit, iit_time - :os.system_time(@unit_time))
  end

  def put_last_block(nil), do: :none

  def put_last_block(last_block) do
    put(:last_block, last_block)
    put(:next_index, last_block.height + 1)
  end

  def genesis_block, do: get(:genesis_block)
  def genesis_time, do: get(:genesis_time, 0)

  def put_genesis_block(block) do
    put(:genesis_block, block)
    put(:genesis_time, block.time)
  end

  def events do
    get(:events, 0)
  end

  def add_events(events) do
    put(:events, get(:events, 0) + events)
  end

  def start do
    Logger.info("Chain starting")

    channel_id = Default.channel()

    # time sync
    iit = IIT.sync()
    put_iit(iit)

    # get last block built
    last_block = last_block()
    my_address = Default.address58()

    case last_block do
      nil ->
        """
        #{channel_id} is ready!
        IIT          #{Format.timestamp(iit)}
        No Blocks
        Host Address #{my_address}
        """
        |> Logger.info()

      last_block ->
        genesis_block = genesis_block()

        """
        #{channel_id} is ready!
        IIT               #{Format.timestamp(iit)}
        Genesis Time      #{Format.timestamp(genesis_block.time)}
        Blockchain height #{last_block.height}
        Host Address      #{my_address}
        """
        |> Logger.info()
    end
  end

  def add_block(prev_block, b, channel_id) do
    is_genesis_block = b.height == 0

    case BlockValidator.valid_block?(prev_block, b) do
      :ok ->
        block =
          b
          |> Map.from_struct()
          |> Map.drop([:events, :__meta__])

        # IO.inspect(block)

        if is_genesis_block do
          put_genesis_block(block)
        end

        put_last_block(block)
        add_events(block.ev_count)
        sync()

        Ecto.Multi.new()
        |> Ecto.Multi.insert_all(:block, Block, [block], prefix: channel_id, returning: false)
        # |> Tx.set_status_complete(:txs, block.height, channel_id)
        # |> Tx.cancel_all_pending(:pending, block.height, channel_id)
        # |> Channel.multi_put_genesis_time(:gen_time, channel_id, block.time, is_genesis_block)
        # |> Channel.multi_update(
        #   :channel,
        #   channel_id,
        #   block.height,
        #   block.hash,
        #   1,
        #   block.ev_count
        # )
        |> Repo.transaction()
        |> case do
          {:ok, _} ->
            :ok

          err ->
            IO.inspect(err)

            # Ecto.Multi.new()
            # |> Tx.cancel_all_pending(:pending, b.index, channel_id)
            # |> Repo.transaction()

            err
        end

      err ->
        IO.inspect(err)

        err
    end
  end
end
