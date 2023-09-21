defmodule DetsPlus do
  @moduledoc """
  DetsPlus persistent tuple/struct/map storage.

  [DetsPlus](https://github.com/dominicletz/dets_plus) has a similiar API as `dets` but without
  the 2GB file storage limit. Writes are buffered in an
  internal ETS table and synced every `auto_save` period
  to the persistent storage.

  While `sync()` or `auto_save` are in progress the database
  is still readable and writeable.

  There is no commitlog so not synced writes are lost.
  Lookups are possible by key and non-matches are accelerated
  using a bloom filter. The persistent file concept follows
  DJ Bernsteins CDB database format, but uses an Elixir
  encoded header https://cr.yp.to/cdb.html. When syncing
  a new CDB database file is created and replaces
  the old CDB atomically file using `File.rename!` so
  database corruptions are not possible from incomplete updates.

  Limits are:
  - Total file size: 18_446 Petabyte
  - Maximum entry size: 4 Gigabyte
  - Maximum entry count: :infinity
  """

  # This limits the total database size to 18_446 PB
  @slot_size 8
  @slot_size_bits @slot_size * 8

  # This limits the biggest entry size to 4 GB
  @entry_size_size 4
  @entry_size_size_bits @entry_size_size * 8

  # We're using sha256 as source - should not have conflicts ever
  @hash_size 8
  @hash_size_bits @hash_size * 8

  # Tuples for use in the hash tables
  @null_tuple {<<0::unsigned-size(64)>>, 0}
  @null_binary <<0::unsigned-size(128)>>

  @version 3

  @start_offset byte_size("DET+")
  @page_cache_memory 1_000_000_000
  @ets_type :set
  @keypos 1

  alias DetsPlus.{Bloom, EntryWriter, FileReader, FileWriter}
  use GenServer

  defstruct [:pid, :ets]

  @type t :: %__MODULE__{pid: pid()}

  defmodule State do
    @moduledoc false
    @enforce_keys [:version]
    defstruct [
      :auto_save,
      :bloom_size,
      :bloom,
      :creation_stats,
      :ets,
      :file_entries,
      :file_size,
      :filename,
      :fp,
      :header_size,
      :mode,
      :name,
      :slot_counts,
      :sync_fallback,
      :sync_waiters,
      :sync,
      :table_offsets,
      :version
    ]
  end

  @doc false
  def start_link(args) do
    DetsPlus.open_file(args[:name], args)
  end

  @doc false
  def start_link(_, args) do
    DetsPlus.open_file(args[:name], args)
  end

  @doc """
    Opens an existing table or creates a new table. If no
    `file` argument is provided the table name will be used.

    Arguments:

    - `auto_save` - The autosave interval. If the interval is an integer Time, the table is flushed to disk whenever it is not accessed for Time milliseconds. If the interval is the atom infinity, autosave is disabled. Defaults to `180_000` (3 minutes).
    - `auto_save_memory` - The autosave threshold in memory. When the internal ETS table reaches a size bigger than this the table is flushed to disk. Defaults to `1_000_000_000` (1 GB)
    - `page_cache_memory` - The amount of memory to use for file system caching. Defaults to `1_000_000_000` (1 GB)
    - `keypos` - The position of the element of each object to be used as key. Defaults to 1. The ability to explicitly state the key position is most convenient when we want to store Erlang records in which the first position of the record is the name of the record type.
  """
  def open_file(name, args \\ []) when is_atom(name) do
    filename = Keyword.get(args, :file, name) |> do_string()
    auto_save = Keyword.get(args, :auto_save, :infinity)
    mode = Keyword.get(args, :access, :read_write)
    var_name = Keyword.get(args, :var, :dets)

    state =
      with true <- File.exists?(filename),
           {:ok, %File.Stat{size: file_size}} when file_size > 0 <- File.stat(filename) do
        load_state(filename, file_size)
      else
        _ ->
          # unused options from dets:
          # - ram_file
          # - max_no_slots
          # - min_no_slots
          # - repair

          %State{
            version: @version,
            bloom: "",
            bloom_size: 0,
            filename: filename,
            fp: nil,
            name: name,
            mode: mode,
            auto_save: auto_save,
            file_entries: 0,
            slot_counts: %{},
            file_size: 0,
            sync: nil,
            sync_waiters: [],
            sync_fallback: nil
          }
      end

    # These properties should override what is stored on disk
    state = %State{
      state
      | auto_save: auto_save,
        mode: mode
    }

    {:ok, pid} = GenServer.start_link(__MODULE__, state, hibernate_after: 5_000, name: name)
    :persistent_term.put(var_name, pid)
    {:ok, pid}
  end

  defmacrop call(pid, cmd, timeout \\ :infinity) do
    quote do
      :gen_server.call(unquote(pid), unquote(cmd), unquote(timeout))
    end
  end

  defmacrop cast(pid, cmd) do
    quote do
      :gen_server.cast(unquote(pid), unquote(cmd))
    end
  end

  defmacro encode(term) do
    quote do
      :erlang.term_to_binary(unquote(term))
    end
  end

  defmacro decode(bin) do
    quote do
      :erlang.binary_to_term(unquote(bin))
    end
  end

  defmacrop default_hash(key) do
    quote do
      <<hash::binary-size(@hash_size), _::binary>> =
        Blake3.hash(unquote(key))

      hash
    end
  end

  if is_integer(@keypos) do
    @keypos_keyfun @keypos - 1
    defp keyfun(tuple) do
      elem(tuple, @keypos_keyfun)
    end

    defp hashfun(tuple) do
      default_hash(elem(tuple, @keypos_keyfun))
    end
  else
    defp keyfun(map) do
      Map.get(map, @keypos)
    end

    defp hashfun(map) do
      default_hash(Map.get(map, @keypos))
    end
  end

  defp load_state(filename, file_size) do
    fp = file_open(filename)

    {:ok, <<header_offset::unsigned-size(@slot_size_bits)>>} =
      PagedFile.pread(fp, file_size - @slot_size, @slot_size)

    {:ok, header} = PagedFile.pread(fp, header_offset, file_size - header_offset - @slot_size)
    %State{version: version, bloom: bloom} = state = :erlang.binary_to_term(header)

    if version != @version do
      raise("incompatible dets+ version #{version}")
    end

    state = %State{
      state
      | fp: fp,
        file_size: file_size,
        header_size: byte_size(header)
    }

    {:ok, bloom} = PagedFile.pread(fp, bloom, header_offset - bloom)

    %State{state | bloom: bloom}
  end

  @wfile PagedFile
  defp store_state(state = %State{fp: fp, bloom: bloom}) do
    bloom_offset = @wfile.size(fp)
    @wfile.pwrite(fp, bloom_offset, bloom)
    header_offset = @wfile.size(fp)

    bin =
      :erlang.term_to_binary(
        %State{
          state
          | version: @version,
            bloom: bloom_offset,
            fp: nil,
            sync: nil,
            sync_waiters: [],
            ets: nil
        },
        [:compressed]
      )

    @wfile.pwrite(fp, header_offset, bin <> <<header_offset::unsigned-size(@slot_size_bits)>>)
    %State{state | file_size: @wfile.size(fp), header_size: byte_size(bin)}
  end

  @impl true
  def init(state = %State{auto_save: auto_save}) do
    if is_integer(auto_save) do
      :timer.send_interval(auto_save, :auto_save)
    end

    {:ok, init_ets(state)}
  end

  defp init_ets(state = %State{name: name, filename: filename}) do
    state_file = ~c"#{filename}.ets"

    ets =
      if File.exists?(state_file) do
        case :ets.file2tab(state_file) do
          {:ok, ets} -> ets
          _ -> File.rm(state_file)
        end
      else
        :ets.new(name, [@ets_type, read_concurrency: true, write_concurrency: true])
      end

    %State{state | ets: ets}
  end

  defp init_table_offsets(state = %State{slot_counts: slot_counts}, start_offset) do
    table_offsets =
      Enum.reduce(1..256, %{0 => start_offset}, fn table_idx, table_offsets ->
        offset =
          Map.get(table_offsets, table_idx - 1) +
            Map.get(slot_counts, table_idx - 1, 0) * (@slot_size + @hash_size)

        Map.put(table_offsets, table_idx, offset)
      end)

    %State{state | table_offsets: table_offsets}
  end

  @doc """
  Syncs pending writes to the persistent file and closes the table.
  """
  @spec close(pid()) :: :ok
  def close(pid) do
    call(pid, :sync)
    GenServer.stop(pid)
  end

  @doc """
  Deletes all objects from a table in almost constant time.
  """
  @spec delete_all_objects(pid()) :: :ok | {:error, atom()}
  def delete_all_objects(pid) do
    call(pid, :delete_all_objects)
  end

  @doc """
  Deletes all instances of a specified object from a table.
  """
  @spec delete_object(pid(), tuple() | map()) :: :ok | {:error, atom()}
  def delete_object(pid, object) do
    delete(pid, keyfun(object))
  end

  @doc """
  Deletes all objects with key Key from table Name.
  """
  @spec delete(pid(), any()) :: :ok | {:error, atom()}
  def delete(pid, key) do
    call(pid, {:insert, [{key, :delete}]})
  end

  @doc """
    Inserts one object into the table. If there already exists an object with a key matching the key of some of the given objects, the old object will be replaced.
  """
  @spec insert(pid(), tuple() | map()) :: :ok | {:error, atom()}
  def insert(pid, object) do
    cast(pid, {:insert, {keyfun(object), object}})
  end

  @doc """
    Inserts one or more objects into the table. If there already exists an object with a key matching the key of some of the given objects, the old object will be replaced.
  """
  @spec multi_insert(pid(), [tuple() | map()]) :: :ok | {:error, atom()}
  def multi_insert(pid, objects) do
    objects =
      List.wrap(objects)
      |> Enum.map(fn object -> {keyfun(object), object} end)

    cast(pid, {:insert, objects})
  end

  def insert_sync(pid, objects) do
    List.wrap(objects)
    |> Enum.map(fn object -> {keyfun(object), object} end)

    call(pid, {:insert, objects})
  end

  @doc """
  Inserts one or more objects into the table. If there already exists an object with a key matching the key of some of the given objects, the old object will be replaced.
  """
  @spec insert_new(pid(), tuple() | map() | [tuple() | map()]) :: true | false
  def insert_new(pid, object) do
    objects =
      List.wrap(object)
      |> Enum.map(fn object -> {keyfun(object), hashfun(object), object} end)

    call(pid, {:insert_new, objects})
  end

  @spec map_put(pid() | atom(), binary, binary, map) :: :ok
  def map_put(pid, key, index, value) do
    case call(pid, {:lookup, key, default_hash(key)}) do
      nil ->
        cast(pid, {:insert, {key, %{index => value}}})

      x ->
        cast(pid, {:insert, {key, Map.put(x, index, value)}})
    end
  end

  @spec update(pid() | atom(), binary, map) :: :ok
  def update(pid, key, value) do
    case call(pid, {:lookup, key, default_hash(key)}) do
      nil ->
        cast(pid, {:insert, {key, value}})

      x ->
        cast(pid, {:insert, {key, Map.merge(x, value)}})
    end
  end

  @spec rollback(pid()) :: :ok
  def rollback(pid) do
    call(pid, :rollback)
  end

  @doc """
  Returns the number of object in the table. This is an estimate and the same as `info(dets, :size)`.
  """
  @spec count(pid()) :: integer()
  # def count(pid) when is_pid(pid) or is_atom(pid), do: count(call(pid, :get_handle))

  def count(pid) do
    info(pid, :size)
  end

  @doc """
  Reducer function following the `Enum` protocol.
  """
  @spec reduce(DetsPlus.t(), any(), fun()) :: any()
  def reduce(dets = %__MODULE__{}, acc, fun) do
    Enum.reduce(dets, acc, fun)
  end

  @spec all(atom | pid) :: list
  def all(pid) do
    call(pid, :get_handle)
    |> Enum.to_list()
  end

  @doc """
  Returns a list of all objects with key Key stored in the table.

  Example:

  ```
  2> State.open_file(:abc)
  {ok,:abc}
  3> State.insert(:abc, {1,2,3})
  ok
  4> State.insert(:abc, {1,3,4})
  ok
  5> State.lookup(:abc, 1).
  [{1,3,4}]
  ```

  If the table type is set, the function returns either the empty list or a list with one object, as there cannot be more than one object with a given key. If the table type is bag or duplicate_bag, the function returns a list of arbitrary length.

  Notice that the order of objects returned is unspecified. In particular, the order in which objects were inserted is not reflected.
  """
  @spec lookup(pid(), any) :: nil | any() | {:error, atom()}
  def lookup(pid, key) do
    call(pid, {:lookup, key, default_hash(key)})
  end

  def lookup(pid, key, default) do
    call(pid, {:lookup, key, default_hash(key)}) || default
  end

  @doc """
  Works like `lookup/2`, but does not return the objects. Returns true if one or more table elements has the key `key`, otherwise false.
  """
  @spec member?(pid(), any) :: false | true | {:error, atom}
  def member?(pid, key) do
    case lookup(pid, key) do
      [] -> false
      list when is_list(list) -> true
      error -> error
    end
  end

  @doc """
  Ensures that all updates made to table are written to disk. While the sync is running the
  table can still be used for reads and writes, but writes issued after the `sync/1` call
  will not be part of the persistent file. These new changes will only be included in the
  next sync call.
  """
  @spec sync(pid()) :: :ok
  def sync(pid) do
    call(pid, :sync)
  end

  @spec commit(pid()) :: :ok
  def commit(pid) do
    call(pid, :commit)
  end

  @doc """
  Starts a sync of all changes to the disk. Same as `sync/1` but doesn't block
  """
  @spec start_sync(pid()) :: :ok
  def start_sync(pid) do
    call(pid, :start_sync)
  end

  @doc """
  Returns information about table Name as a list of objects:

  - `{file_size, integer() >= 0}}` - The file size, in bytes.
  - `{filename, file:name()}` - The name of the file where objects are stored.
  - `{keypos, keypos()}` - The key position.
  - `{size, integer() >= 0}` - The number of objects estimated in the table.
  - `{type, type()}` - The table type.
  """
  @spec info(pid()) :: [] | nil
  # def info(%__MODULE__{pid: pid}), do: info(pid)

  def info(pid) do
    call(pid, :info)
  end

  def get_handle(pid) do
    GenServer.call(pid, :get_handle)
  end

  @doc """
  Returns the information associated with `item` for the table. The following items are allowed:

  - `{file_size, integer() >= 0}}` - The file size, in bytes.
  - `{header_size, integer() >= 0}}` - The size of erlang term encoded header.
  - `{bloom_bytes, integer() >= 0}}` - The size of the in-memory and on-disk bloom filter, in bytes.
  - `{hashtable_bytes, integer() >= 0}}` - The size of the on-disk lookup hashtable, in bytes.
  - `{filename, file:name()}` - The name of the file where objects are stored.
  - `{keypos, keypos()}` - The key position.
  - `{size, integer() >= 0}` - The number of objects estimated in the table.
  - `{type, type()}` - The table type.
  """
  @spec info(
          pid(),
          :file_size
          | :header_size
          | :filename
          | :keypos
          | :size
          | :type
          | :creation_stats
          | :bloom_bytes
          | :hashtable_bytes
        ) ::
          any()
  def info(pid, item)
      when item == :file_size or item == :filename or item == :keypos or item == :size or
             item == :type or item == :creation_stats do
    case info(pid) do
      nil -> nil
      list -> Keyword.get(list, item)
    end
  end

  @impl true
  def handle_call(
        :get_handle,
        _from,
        state = %State{ets: ets}
      ) do
    {:reply, %DetsPlus{pid: self(), ets: ets}, state}
  end

  def handle_call(
        {:insert, objects},
        _from,
        state = %State{ets: ets, sync: nil}
      ) do
    :ets.insert(ets, objects)

    {:reply, :ok, state}
  end

  def handle_call(
        {:insert, objects},
        _from,
        state = %State{sync_fallback: fallback}
      ) do
    :ets.insert(fallback, objects)

    {:reply, :ok, state}
  end

  def handle_call(
        :delete_all_objects,
        _from,
        state = %State{fp: fp, ets: ets, sync: sync, sync_waiters: waiters, filename: filename}
      ) do
    :ets.delete_all_objects(ets)

    if is_pid(sync) do
      Process.unlink(sync)
      Process.exit(sync, :kill)
    end

    for w <- waiters do
      :ok = GenServer.reply(w, :ok)
    end

    if fp != nil do
      PagedFile.close(fp)
    end

    PagedFile.delete(filename)

    {:reply, :ok,
     %State{
       state
       | sync: nil,
         sync_waiters: [],
         sync_fallback: nil,
         fp: nil,
         bloom: "",
         bloom_size: 0,
         file_entries: 0,
         slot_counts: %{},
         file_size: 0
     }}
  end

  def handle_call(
        :get_reduce_state,
        _from,
        state = %State{sync_fallback: fallback, filename: filename}
      ) do
    {:reply, {fallback, filename}, state}
  end

  def handle_call({:insert_new, objects}, from, state) do
    exists =
      Enum.any?(objects, fn {key, hash, _object} ->
        do_lookup(key, hash, state) != []
      end)

    if exists do
      {:reply, false, state}
    else
      objects = Enum.map(objects, fn {key, _hash, object} -> {key, object} end)
      # handle_call({:insert, objects}, from, state)
      handle_call({:insert, objects}, from, state)
      {:reply, true, state}
    end
  end

  def handle_call({:lookup, key, hash}, _from, state) do
    {:reply, do_lookup(key, hash, state), state}
  end

  def handle_call(
        :info,
        _from,
        state = %State{
          bloom: bloom,
          filename: filename,
          ets: ets,
          file_size: file_size,
          header_size: header_size,
          file_entries: file_entries,
          creation_stats: creation_stats,
          slot_counts: slot_counts
        }
      ) do
    size = :ets.info(ets, :size) + file_entries
    table_size = Enum.sum(Map.values(slot_counts)) * @slot_size + @hash_size

    info = [
      file_size: file_size,
      header_size: header_size,
      bloom_bytes: byte_size(bloom),
      hashtable_bytes: table_size,
      filename: filename,
      keypos: @keypos,
      size: size,
      type: @ets_type,
      creation_stats: creation_stats
    ]

    {:reply, info, state}
  end

  def handle_call(:start_sync, _from, state = %State{sync: sync, ets: ets}) do
    sync =
      if sync == nil and :ets.info(ets, :size) > 0 do
        spawn_sync_worker(state)
      else
        sync
      end

    {:reply, :ok, %State{state | sync: sync}}
  end

  def handle_call(
        :commit,
        _from,
        state = %State{ets: ets, filename: filename}
      ) do
    cond do
      :ets.info(ets, :size) == 0 ->
        {:reply, :ok, state}

      true ->
        tmp_file = ~c"#{filename}.ets.tmp"
        :file.delete(tmp_file)

        fun =
          fn ->
            GenServer.cast(self(), :commit_begin)
            :ets.tab2file(ets, tmp_file)
            :file.rename(tmp_file, ~c"#{filename}.ets")
            GenServer.cast(self(), :commit_complete)
          end

        {:reply, fun, state}
    end
  end

  def handle_call(:sync, from, state = %State{name: name, sync: nil, ets: ets}) do
    if :ets.info(ets, :size) == 0 do
      {:reply, :ok, state}
    else
      sync = spawn_sync_worker(state)

      {:noreply,
       %State{
         state
         | sync: sync,
           sync_fallback:
             :ets.new(name, [@ets_type, read_concurrency: true, write_concurrency: true]),
           sync_waiters: [from]
       }}
    end
  end

  def handle_call(:sync, from, state = %State{sync_waiters: sync_waiters}) do
    {:noreply, %State{state | sync_waiters: [from | sync_waiters]}}
  end

  def handle_call(:rollback, _from, state = %State{ets: ets, sync: nil}) do
    :ets.delete_all_objects(ets)
    {:reply, :ok, state}
  end

  def handle_call(:rollback, _from, state = %State{sync: fallback}) do
    :ets.delete_all_objects(fallback)
    {:reply, :ok, state}
  end

  defp do_lookup(key, hash, state = %State{ets: ets, sync: nil}) do
    case :ets.lookup(ets, key) do
      [] -> file_lookup(state, key, hash)
      [{_key, :delete}] -> nil
      [{_key, object}] -> object
    end
  end

  defp do_lookup(key, hash, state = %State{ets: ets, sync: fallback}) do
    case :ets.lookup(fallback, key) do
      [] ->
        case :ets.lookup(ets, key) do
          [] -> file_lookup(state, key, hash)
          [{_key, :delete}] -> nil
          [{_key, object}] -> object
        end

      [{_key, :delete}] ->
        nil

      [{_key, object}] ->
        object
    end
  end

  @impl true
  def handle_cast(
        {:insert, objects},
        state = %State{ets: ets, sync: nil}
      ) do
    :ets.insert(ets, objects)

    {:noreply, state}
  end

  def handle_cast(
        {:insert, objects},
        state = %State{sync_fallback: fallback}
      ) do
    :ets.insert(fallback, objects)

    {:noreply, state}
  end

  def handle_cast(
        {:sync_complete, sync_pid, new_filename, new_state = %State{}},
        %State{
          fp: fp,
          filename: filename,
          ets: ets,
          sync: sync_pid,
          sync_waiters: waiters,
          sync_fallback: fallback
        }
      ) do
    if fp != nil do
      :ok = PagedFile.close(fp)
    end

    File.rename!(new_filename, filename)
    fp = file_open(filename)

    :ets.delete_all_objects(ets)
    :ets.insert(ets, :ets.tab2list(fallback))
    :ets.delete(fallback)
    :file.delete(~c"#{filename}.ets")

    for w <- waiters do
      :ok = GenServer.reply(w, :ok)
    end

    {:noreply,
     %State{
       new_state
       | fp: fp,
         sync: nil,
         sync_fallback: nil,
         sync_waiters: []
     }}
  end

  # this is pending sync finishing while a complete delete_all_objects has been executed on the state
  def handle_cast({:sync_complete, _sync_pid, _new_filename, _new_state}, state = %State{}) do
    {:noreply, state}
  end

  @impl true
  def handle_info(:auto_save, state = %State{sync: sync, ets: ets}) do
    sync =
      if sync == nil and :ets.info(ets, :size) > 0 do
        spawn_sync_worker(state)
      else
        sync
      end

    {:noreply, %State{state | sync: sync}}
  end

  defp add_stats({prev, stats}, label) do
    now = :erlang.timestamp()
    elapsed = div(:timer.now_diff(now, prev), 1000)
    # IO.puts("#{label} #{elapsed}ms")
    {now, [{label, elapsed} | stats]}
  end

  defp spawn_sync_worker(
         state = %State{
           ets: ets,
           fp: fp,
           filename: filename,
           file_entries: file_entries
           #  keyhashfun: keyhashfun,
           #  keyfun: keyfun
         }
       ) do
    # assumptions here
    # 1. ets data set is small enough to fit into memory
    # 2. fp entries are sorted by hash
    dets = self()

    spawn_link(fn ->
      Process.flag(:priority, :low)
      register_name()
      stats = {:erlang.timestamp(), []}
      new_dataset = :ets.tab2list(ets)
      stats = add_stats(stats, :ets_flush)

      # Ensuring hash function sort order
      new_dataset = parallel_hash(new_dataset)
      stats = add_stats(stats, :ets_hash)

      old_file =
        if fp != nil do
          FileReader.new(fp, @start_offset, module: PagedFile, buffer_size: 100_000)
        else
          nil
        end

      new_filename = "#{filename}.tmp"
      @wfile.delete(new_filename)

      # ~5mb for this write buffer, it's mostly append only, higher values
      # didn't make an impact.
      # (the only non-append workflow on this fp is the hash overflow handler, but that is usually small)
      opts = [page_size: 512_000, max_pages: 10, priority: :low]
      {:ok, new_file} = @wfile.open(new_filename, opts)
      state = %State{state | fp: new_file}
      stats = add_stats(stats, :fopen)
      new_dataset_length = length(new_dataset)

      # setting the bloom size based of a size estimate
      future_bloom = task_async(fn -> write_bloom(file_entries + new_dataset_length) end)
      future_entries = task_async(fn -> write_entries(state, new_dataset_length) end)
      future_state = task_async(fn -> write_data(state) end)

      # This starts of the file_reader sending entry data to above the future_* workers
      pids = [future_bloom.pid, future_entries.pid, future_state.pid]
      async_iterate_produce(new_dataset, old_file, pids)

      state = Task.await(future_state, :infinity)
      {bloom, bloom_size} = Task.await(future_bloom, :infinity)
      state = %State{state | bloom: bloom, bloom_size: bloom_size}

      entries = Task.await(future_entries, :infinity)

      stats = add_stats(stats, :write_entries)
      state = write_hashtable(state, entries)
      stats = add_stats(stats, :write_hashtable)
      state = store_state(state)
      stats = add_stats(stats, :header_store)

      @wfile.close(new_file)
      {_, stats} = add_stats(stats, :file_close)

      state = %State{state | creation_stats: stats}
      GenServer.cast(dets, {:sync_complete, self(), new_filename, state})
    end)

    # Profiler.fprof(worker)
  end

  @max 1
  defp register_name(n \\ 0) do
    n =
      if n == @max do
        0
      else
        n + 1
      end

    try do
      Process.register(self(), String.to_atom("DetsPlus_Flush_#{n}"))
    rescue
      _e ->
        Process.sleep(100)
        register_name(n)
    end
  end

  @min_chunk_size 10_000
  @max_tasks 4
  @doc false
  def parallel_hash(new_dataset, tasks \\ 1) do
    len = length(new_dataset)

    if len > @min_chunk_size and tasks < @max_tasks do
      {a, b} = Enum.split(new_dataset, div(len, 2))
      task = task_async(fn -> parallel_hash(a, tasks * 2) end)
      result = parallel_hash(b, tasks * 2)
      :lists.merge(Task.await(task, :infinity), result)
    else
      Enum.map(new_dataset, fn {key, object} ->
        {{default_hash(key), key}, object}
      end)
      |> :lists.sort()
    end
  end

  defp task_async(fun) do
    Task.async(fn ->
      Process.flag(:priority, :low)
      fun.()
    end)
  end

  defp write_bloom(estimated_file_entries) do
    bloom_size = estimated_file_entries * 10

    Bloom.create(bloom_size)
    |> async_iterate_consume(fn bloom, entry_hash, _entry, _ ->
      {:cont, Bloom.add(bloom, entry_hash)}
    end)
    |> Bloom.finalize()
  end

  defp write_entries(
         %State{file_entries: old_file_entries, filename: filename},
         new_dataset_length
       ) do
    estimated_entry_count = new_dataset_length + old_file_entries
    entries = EntryWriter.new(filename, estimated_entry_count)

    {entries, _offset} =
      async_iterate_consume({entries, 4}, fn {entries, offset},
                                             entry_hash,
                                             entry_bin,
                                             _entry_term ->
        size = byte_size(entry_bin)
        table_idx = table_idx(entry_hash)

        entries =
          EntryWriter.insert(
            entries,
            {table_idx, entry_hash, offset + @hash_size}
          )

        offset = offset + @hash_size + @entry_size_size + size
        {:cont, {entries, offset}}
      end)

    entries
  end

  defp write_data(state = %State{fp: fp}) do
    state = %State{state | file_entries: 0, slot_counts: %{}}
    writer = FileWriter.new(fp, 0, module: @wfile)
    writer = FileWriter.write(writer, "DET+")

    {state, writer} =
      async_iterate_consume(
        {state, writer},
        fn {state = %State{file_entries: file_entries, slot_counts: slot_counts}, writer},
           entry_hash,
           entry_bin,
           _entry_term ->
          table_idx = table_idx(entry_hash)
          slot_counts = Map.update(slot_counts, table_idx, 1, fn count -> count + 1 end)
          state = %State{state | file_entries: file_entries + 1, slot_counts: slot_counts}
          size = byte_size(entry_bin)

          writer =
            FileWriter.write(
              writer,
              <<entry_hash::binary-size(@hash_size), size::unsigned-size(@entry_size_size_bits),
                entry_bin::binary>>
            )

          {:cont, {state, writer}}
        end
      )

    # final zero offset after all data entries
    FileWriter.write(
      writer,
      <<0::unsigned-size(@hash_size_bits), 0::unsigned-size(@entry_size_size_bits)>>
    )
    |> FileWriter.sync()

    state
  end

  _ =
    Enum.reduce(1..56, {[], 2}, fn bits, {code, size} ->
      next =
        defp slot_idx(unquote(size), <<_, slot::unsigned-size(unquote(bits)), _::bitstring>>) do
          slot
        end

      {[next | code], size * 2}
    end)
    |> elem(0)

  defp write_hashtable(state = %State{slot_counts: slot_counts, fp: fp}, entries) do
    new_slot_counts =
      Enum.map(slot_counts, fn {key, value} ->
        {key, next_power_of_two(trunc(value * 1.3) + 1)}
      end)
      |> Map.new()

    state =
      %State{state | slot_counts: new_slot_counts}
      |> init_table_offsets(@wfile.size(fp))

    Enum.chunk_every(0..255, 64)
    |> Enum.map(fn range ->
      start_offset = table_offset(state, hd(range))
      end_offset = table_offset(state, List.last(range) + 1)
      writer = FileWriter.new(fp, start_offset, module: @wfile, limit: end_offset)
      task_async(fn -> write_hashtable_range(range, writer, entries, new_slot_counts) end)
    end)
    |> Task.await_many(:infinity)

    EntryWriter.close(entries)
    state
  end

  defp write_hashtable_range(range, writer, entries, slot_counts) do
    Enum.reduce(range, writer, fn table_idx, writer ->
      slot_count = Map.get(slot_counts, table_idx, 0)
      entries = EntryWriter.lookup(entries, table_idx)
      start_offset = FileWriter.offset(writer)
      {writer, overflow} = reduce_entries(entries, writer, slot_count)

      if overflow == [] do
        writer
      else
        writer = FileWriter.sync(writer)

        reduce_overflow(
          Enum.reverse(overflow),
          FileReader.new(writer.fp, start_offset, module: @wfile)
        )

        writer
      end
    end)
    |> FileWriter.sync()
  end

  defp next_power_of_two(n), do: next_power_of_two(n, 2)
  defp next_power_of_two(n, x) when n < x, do: x
  defp next_power_of_two(n, x), do: next_power_of_two(n, x * 2)

  defp reduce_entries(entries, writer, slot_count),
    do: reduce_entries(entries, writer, slot_count, -1, [])

  defp reduce_entries([], writer, slot_count, last_slot, overflow)
       when last_slot + 1 == slot_count,
       do: {writer, overflow}

  defp reduce_entries([], writer, slot_count, last_slot, overflow) do
    writer = FileWriter.write(writer, @null_binary)
    reduce_entries([], writer, slot_count, last_slot + 1, overflow)
  end

  defp reduce_entries(
         [
           <<entry_hash::binary-size(8), _offset::unsigned-size(@slot_size_bits)>> = entry
           | entries
         ],
         writer,
         slot_count,
         last_slot,
         overflow
       ) do
    slot_idx = slot_idx(slot_count, entry_hash)

    cond do
      last_slot + 1 == slot_count ->
        reduce_entries(entries, writer, slot_count, last_slot, [entry | overflow])

      last_slot + 1 >= slot_idx ->
        writer = FileWriter.write(writer, entry)
        reduce_entries(entries, writer, slot_count, last_slot + 1, overflow)

      true ->
        writer = FileWriter.write(writer, @null_binary)
        reduce_entries([entry | entries], writer, slot_count, last_slot + 1, overflow)
    end
  end

  defp reduce_overflow([], _reader), do: :ok

  defp reduce_overflow([entry | overflow], reader) do
    curr = FileReader.offset(reader)
    {reader, next} = FileReader.read(reader, @slot_size + @hash_size)

    case next do
      @null_binary ->
        @wfile.pwrite(reader.fp, curr, entry)
        reduce_overflow(overflow, reader)

      _ ->
        reduce_overflow([entry | overflow], reader)
    end
  end

  @async_send_buffer_size 10_000_000
  @async_send_buffer_trigger div(10_000_000, 2)
  defp async_iterate_produce(
         new_dataset,
         file_reader,
         targets
       ) do
    spawn_link(fn ->
      init_acc = {[], 0, targets, -@async_send_buffer_trigger}

      {:done, {items, _item_count, _targets, _byte_count}} =
        iterate({:cont, init_acc}, new_dataset, file_reader, &async_iterator/4)

      for pid <- targets do
        send(pid, {:entries, self(), Enum.reverse(items)})
        send(pid, :done)
      end
    end)
  end

  defp async_iterator(state, _entry_hash, _binary_entry, :delete) do
    {:cont, state}
  end

  defp async_iterator(
         {items, item_count, targets, byte_count},
         entry_hash,
         entry_bin,
         entry_term
       ) do
    items = [{entry_hash, entry_bin, entry_term} | items]
    item_count = item_count + 1
    byte_count = byte_count + byte_size(entry_bin)

    if item_count > 128 or byte_count >= @async_send_buffer_size do
      for pid <- targets do
        send(pid, {:entries, self(), Enum.reverse(items)})
      end

      {:cont, {[], 0, targets, await_processing(byte_count, targets)}}
    else
      {:cont, {items, item_count, targets, byte_count}}
    end
  end

  defp await_processing(byte_count, targets) do
    if byte_count >= @async_send_buffer_size do
      for pid <- targets do
        receive do
          {^pid, :continue} -> :ok
        end
      end

      await_processing(byte_count - @async_send_buffer_size, targets)
    else
      byte_count
    end
  end

  defp async_iterate_consume(acc, fun, byte_count0 \\ 0) do
    receive do
      {:entries, producer, entries} ->
        {acc, byte_count} =
          Enum.reduce(entries, {acc, byte_count0}, fn {entry_hash, entry_bin, entry_term},
                                                      {acc, byte_count} ->
            byte_count = byte_count + byte_size(entry_bin)
            {:cont, acc} = fun.(acc, entry_hash, entry_bin, entry_term)
            {acc, byte_count}
          end)

        async_iterate_consume(acc, fun, confirm_processing(byte_count, producer))

      :done ->
        acc
    end
  end

  defp confirm_processing(byte_count, producer) do
    if byte_count >= @async_send_buffer_size do
      send(producer, {self(), :continue})
      confirm_processing(byte_count - @async_send_buffer_size, producer)
    else
      byte_count
    end
  end

  defp read_next_entry(nil), do: nil

  defp read_next_entry(file_reader) do
    case FileReader.read(file_reader, @hash_size + @entry_size_size) do
      {_new_file_reader,
       <<0::unsigned-size(@hash_size_bits), 0::unsigned-size(@entry_size_size_bits)>>} ->
        nil

      {new_file_reader,
       <<entry_hash::binary-size(@hash_size), old_size::unsigned-size(@entry_size_size_bits)>>} ->
        {file_reader, entry_bin} = FileReader.read(new_file_reader, old_size)
        {file_reader, entry_hash, entry_bin}
    end
  end

  # this function takes a new_dataset and an old file and merges them, it calls
  # on every entry the callback `fun.(acc, entry_hash, entry_bin, entry_term)` and returns the final acc
  @doc false
  def iterate({:cont, acc}, new_dataset, file_reader, fun) do
    do_iterate({:cont, acc}, new_dataset, read_next_entry(file_reader), fun)
  end

  defp do_iterate({:halt, acc}, _new_dataset, _old_dataset, _fun) do
    {:halted, acc}
  end

  # Nothing left
  defp do_iterate({:cont, acc}, [], nil, _fun) do
    {:done, acc}
  end

  # Only old entries left
  defp do_iterate({:cont, acc}, [], {fr, entry_hash, entry_bin}, fun) do
    acc = fun.(acc, entry_hash, entry_bin, nil)
    do_iterate(acc, [], read_next_entry(fr), fun)
  end

  # Only new entries left
  defp do_iterate({:cont, acc}, new_dataset, nil, fun) do
    {{entry_hash, _entry_key}, entry_term} = hd(new_dataset)
    entry_bin = encode(entry_term)
    acc = fun.(acc, entry_hash, entry_bin, entry_term)
    do_iterate(acc, tl(new_dataset), nil, fun)
  end

  # Both types are still there
  defp do_iterate({:cont, acc}, new_dataset, {fr, old_entry_hash, old_entry_bin}, fun) do
    # Reading a new entry from the top of the dataset
    {{new_entry_hash, new_entry_key}, new_entry_term} = hd(new_dataset)

    # Reading a new entry from the file or falling back to else if there is no next
    # entry in the file anymore
    case compare({new_entry_hash, new_entry_key}, {old_entry_hash, old_entry_bin}) do
      :equal ->
        entry_bin = encode(new_entry_term)
        acc = fun.(acc, new_entry_hash, entry_bin, new_entry_term)
        do_iterate(acc, tl(new_dataset), read_next_entry(fr), fun)

      :new ->
        entry_bin = encode(new_entry_term)
        acc = fun.(acc, new_entry_hash, entry_bin, new_entry_term)
        do_iterate(acc, tl(new_dataset), {fr, old_entry_hash, old_entry_bin}, fun)

      :old ->
        acc = fun.(acc, old_entry_hash, old_entry_bin, nil)
        do_iterate(acc, new_dataset, read_next_entry(fr), fun)
    end
  end

  defp compare(
         {new_entry_hash, new_entry_key},
         {old_entry_hash, old_entry_bin}
       ) do
    case {new_entry_hash, old_entry_hash} do
      {same, same} ->
        # hash collision should be really seldom, or this is going to be expensive
        old_entry_key = keyfun(decode(old_entry_bin))

        case {new_entry_key, old_entry_key} do
          {same, same} -> :equal
          {new, old} when new < old -> :new
          {new, old} when new > old -> :old
        end

      {new, old} when new < old ->
        :new

      {new, old} when new > old ->
        :old
    end
  end

  defp table_offset(%State{table_offsets: nil}, _table_idx) do
    nil
  end

  defp table_offset(%State{table_offsets: table_offsets}, table_idx) do
    Map.get(table_offsets, table_idx)
  end

  defp file_lookup(%State{file_entries: 0}, _key, _hash), do: nil

  defp file_lookup(state = %State{slot_counts: slot_counts}, key, hash) do
    table_idx = table_idx(hash)
    slot_count = Map.get(slot_counts, table_idx, 0)

    if Bloom.lookup(state, hash) and slot_count > 0 do
      slot = slot_idx(slot_count, hash)

      {ret, _n} =
        file_lookup_slot_loop(state, key, hash, table_offset(state, table_idx), slot, slot_count)

      ret
    else
      nil
    end
  end

  defp batch_read(fp, point, count) do
    {:ok, data} = PagedFile.pread(fp, point, (@slot_size + @hash_size) * count)

    for <<hash::binary-size(@hash_size), offset::unsigned-size(@slot_size_bits) <- data>> do
      {hash, offset}
    end ++ [@null_tuple]
  end

  @batch_size 32
  defp file_lookup_slot_loop(
         state = %State{fp: fp},
         key,
         hash,
         base_offset,
         slot,
         slot_count,
         n \\ 0
       ) do
    slot = rem(slot, slot_count)
    point = base_offset + slot * (@slot_size + @hash_size)
    batch_size = min(@batch_size, slot_count - slot)
    hash_offsets = batch_read(fp, point, batch_size)

    # a zero offset is an indication of the end
    # a hash bigger than the searched hash means
    # we reached the next entry or and overflow entry
    hash_offsets =
      Enum.take_while(hash_offsets, fn {<<khash::binary-size(@hash_size)>>, offset} ->
        offset != 0 and khash <= hash
      end)

    len = length(hash_offsets)

    offsets =
      Enum.filter(hash_offsets, fn {rhash, _offset} ->
        rhash == hash
      end)
      |> Enum.map(fn {_hash, offset} -> offset end)

    {:ok, sizes} =
      PagedFile.pread(fp, Enum.zip(offsets, List.duplicate(@entry_size_size, length(offsets))))

    sizes = Enum.map(sizes, fn <<size::unsigned-size(@entry_size_size_bits)>> -> size end)
    offsets = Enum.map(offsets, fn offset -> offset + @entry_size_size end)
    {:ok, entries} = PagedFile.pread(fp, Enum.zip(offsets, sizes))

    Enum.find_value(entries, fn entry ->
      entry = decode(entry)

      if keyfun(entry) == key do
        entry
      end
    end)
    |> case do
      nil ->
        if len < batch_size do
          {nil, n}
        else
          file_lookup_slot_loop(
            state,
            key,
            hash,
            base_offset,
            slot + batch_size,
            slot_count,
            n + 1
          )
        end

      entry ->
        {entry, n}
    end
  end

  # get a table idx from the hash value
  defp table_idx(<<idx, _::binary>>) do
    idx
  end

  defp do_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp do_string(list) when is_list(list), do: List.to_string(list)
  defp do_string(string) when is_binary(string), do: string

  @file_otps (if(@page_cache_memory > 10_000_000) do
                max_pages = div(@page_cache_memory, 1_000_000)
                [page_size: 1_000_000, max_pages: max_pages]
              else
                max_pages = div(@page_cache_memory, 100_000) |> min(10)
                [page_size: 100_000, max_pages: max_pages]
              end)

  defp file_open(filename) do
    {:ok, fp} = PagedFile.open(filename, @file_otps)
    fp
  end

  @impl true
  def terminate(_reason, %State{fp: fp, ets: ets}) do
    :ets.delete(ets)

    if fp != nil do
      :ok = PagedFile.close(fp)
    end
  end
end

defimpl Enumerable, for: DetsPlus do
  alias DetsPlus.FileReader
  import DetsPlus, only: [decode: 1]

  @start_offset byte_size("DET+")
  def count(_pid), do: {:error, __MODULE__}
  def member?(_pid, _key), do: {:error, __MODULE__}
  def slice(_pid), do: {:error, __MODULE__}

  def reduce(%DetsPlus{pid: pid, ets: ets}, acc, fun) do
    new_data =
      :ets.tab2list(ets)
      |> Enum.reduce(%{}, fn {key, object}, map -> Map.put(map, key, object) end)

    {new_data2, filename} = GenServer.call(pid, :get_reduce_state)
    new_data = Map.merge(new_data, new_data2)

    opts = [page_size: 1_000_000, max_pages: 1000]

    {fp, old_file} =
      with true <- File.exists?(filename),
           {:ok, fp} <- PagedFile.open(filename, opts) do
        old_file = FileReader.new(fp, @start_offset, module: PagedFile, buffer_size: 512_000)
        {fp, old_file}
      else
        _ -> {nil, nil}
      end

    # Ensuring hash function sort order
    new_dataset = DetsPlus.parallel_hash(Map.to_list(new_data))

    ret =
      DetsPlus.iterate(acc, new_dataset, old_file, fn acc, _entry_hash, entry_blob, entry ->
        if entry != :delete do
          fun.(entry || decode(entry_blob), acc)
        else
          {:cont, acc}
        end
      end)

    if fp != nil, do: PagedFile.close(fp)
    ret
  end
end
