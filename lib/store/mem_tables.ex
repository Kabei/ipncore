defmodule MemTables do
  use GenServer

  @ordered_named_opts [
    :ordered_set,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]
  @set_named_concurrent_opts [
    :set,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]
  @set_named_opts [:set, :named_table, :public, read_concurrency: true, write_concurrency: false]

  @tables_name %{
    dhash: "dhash",
    msg: "msg",
    dmsg: "dmsg",
    # used after process round
    dtx: "dtx",
    # cache
    token: "token",
    validator: "validator"
  }

  @tables_opt %{
    msg: @ordered_named_opts,
    dmsg: @ordered_named_opts,
    hash: @set_named_opts,
    dhash: @set_named_opts,
    dtx: @ordered_named_opts,
    # cache
    validator: @set_named_concurrent_opts,
    token: @set_named_concurrent_opts,
    env: @set_named_concurrent_opts
  }

  @save_extension "save"
  # @tmp_extension "save.tmp"

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(state) do
    for {table, opts} <- @tables_opt do
      :ets.new(table, opts)
    end

    load_all()

    Process.flag(:trap_exit, true)

    {:ok, state, :hibernate}
  end

  defmacrop default_dir(basename, extension) do
    quote do
      ~c"#{:persistent_term.get(:save_dir)}#{unquote(basename)}.#{unquote(extension)}"
    end
  end

  def load_all do
    for {table, name} <- @tables_name do
      :ets.file2tab(table, default_dir(name, @save_extension))
    end
  end

  def save(table) do
    name = Map.get(@tables_name, table)
    save(table, name)
  end

  def save(table, name) do
    :ets.tab2file(table, default_dir(name, @save_extension))
  end

  def save_all do
    for {table, name} <- @tables_name do
      save(table, name)
    end
  end

  def delete_all do
    for {table, _name} <- @tables_name do
      :ets.delete(table)
    end
  end

  # def clear_cache do
  # :ets.delete_all_objects(:hash)
  # :ets.delete_all_objects(:dhash)
  # :ets.delete_all_objects(:validator)
  # :ets.delete_all_objects(:token)
  # end

  @impl true
  def terminate(_reason, _state) do
    save_all()
    delete_all()
    :persistent_term.erase(:save_dir)
  end
end
