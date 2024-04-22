defmodule MemPool do
  alias Ippan.Funcs
  use GenServer
  @name :mempool
  @compile {:inline, [get: 0, add: 3]}

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @name)
  end

  @impl true
  def init(args) do
    Process.flag(:trap_exit, true)
    load()
    {:ok, args, :hibernate}
  end

  @impl true
  def terminate(_reason, _state) do
    save()
    :persistent_term.erase(@name)
  end

  def new do
    tid = :ets.new(:msg, [:set])
    tid2 = :ets.new(:block, [:ordered_set])
    cref = :counters.new(1, [:write_concurrency])
    :persistent_term.put(@name, {tid, tid2, cref})
  end

  def get do
    :persistent_term.get(@name)
  end

  def add({tid, tid2, cref}, tx, bs) do
    :counters.add(cref, 1, 1)
    ix = :counters.get(cref, 1)
    key = :erlang.element(1, tx)
    :ets.insert(tid2, {ix, key, bs})
    :ets.insert(tid, tx)
  end

  def size({tid, _, _}) do
    :ets.info(tid, :size)
  end

  def select({tid, tid2, _cref}, max_size) do
    # 1. size count
    # 2. tx count
    cref = :counters.new(2, [])
    first = :ets.first(tid2)
    acc_block = :ets.new(:tmpa, [:duplicate_bag])
    acc_txs = :ets.new(:tmpb, [:bag])

    do_select(first, {tid2, tid}, {acc_block, acc_txs}, cref, max_size)
  end

  defp do_select(
         :"$end_of_table",
         _ets,
         {acc_block, acc_txs},
         cref,
         _max_size
       ) do
    txs_task =
      Task.async(fn ->
        :ets.tab2list(acc_txs)
        |> Enum.group_by(
          fn {x, _y} -> x end,
          fn {_x, y} -> y end
        )
      end)

    block_task =
      Task.async(fn ->
        # :ets.fun2ms(fn {_, x} -> x end)
        :ets.select(acc_block, [{{:_, :_, :"$1"}, [], [:"$1"]}])
      end)

    :ets.delete(acc_block)
    :ets.delete(acc_txs)

    {run_task(block_task), run_task(txs_task), cref}
  end

  defp do_select(
         ix,
         {ets_block, ets_txs} = a,
         {acc_msg, acc_decode} = b,
         cref,
         max_size
       ) do
    [x = {_ix, key, _body_sig}] = :ets.lookup(ets_block, ix)
    [{_key, tx}] = :ets.lookup(ets_txs, key)

    size = :erlang.element(6, tx)
    :counters.add(cref, 1, size)

    cond do
      :counters.get(cref, 1) > max_size ->
        do_select(:"$end_of_table", a, b, cref, max_size)

      true ->
        type_id = :erlang.element(2, tx)
        %{priority: priority} = Funcs.lookup(type_id)
        :ets.insert(acc_msg, x)
        :ets.insert(acc_decode, {priority, tx})

        :ets.delete(ets_block, ix)
        :ets.delete(ets_txs, key)

        :counters.add(cref, 2, 1)
        next = :ets.next(ets_block, ix)
        do_select(next, a, b, cref, max_size)
    end
  end

  defp run_task(t) do
    Task.await(t, :infinity)
  end

  @filename ~c"mempool.data"
  @filename2 ~c"mempool_bs.data"
  # Create and load mempool table and counter
  defp load do
    dir = :persistent_term.get(:save_dir)
    filepath = :filename.join(dir, @filename)
    filepath2 = :filename.join(dir, @filename2)

    if File.exists?(filepath) do
      Task.async(fn ->
        with {:ok, tid} <- :ets.file2tab(filepath),
             {:ok, tid2} <- :ets.file2tab(filepath2) do
          cref = :counters.new(1, [:write_concurrency])
          ix = :ets.last(tid)

          if is_number(ix) do
            :counters.put(cref, 1, ix)
          end

          :persistent_term.put(@name, {tid, tid2, cref})
        else
          _ ->
            new()
        end

        File.rm(filepath)
        File.rm(filepath2)
      end)
      |> run_task()
    else
      new()
    end
  end

  # Save mempool in two files
  defp save do
    {tid, tid2, _cref} = get()

    if :ets.info(tid, :size) != 0 do
      dir = :persistent_term.get(:save_dir)
      filepath = :filename.join(dir, @filename)
      filepath2 = :filename.join(dir, @filename2)
      :ets.tab2file(tid, filepath)
      :ets.tab2file(tid2, filepath2)
    end
  end
end
