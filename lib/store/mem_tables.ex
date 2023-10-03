defmodule MemTables do
  @set_named_concurrent_opts [
    :set,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]
  @set_named_opts [:set, :named_table, :public, read_concurrency: true, write_concurrency: false]
  @dbag_named_opts [
    :duplicate_bag,
    :named_table,
    :public,
    read_concurrency: true,
    write_concurrency: true
  ]

  @tables_name %{
    dhash: "dhash",
    msg: "msg",
    dmsg: "dmsg",
    # used after process round
    dtx: "dtx",
    # cache
    token: "token",
    validator: "validator",
    env: "env"
  }

  @tables_opt %{
    msg: @dbag_named_opts,
    dmsg: @dbag_named_opts,
    dhash: @set_named_opts,
    dtx: @set_named_concurrent_opts,
    # cache
    validator: @set_named_concurrent_opts,
    token: @set_named_concurrent_opts,
    env: @set_named_concurrent_opts
  }

  @tables Map.to_list(@tables_name)

  @save_extension "save"
  # @tmp_extension "save.tmp"

  def child_spec(args) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :init, [args]}
    }
  end

  def init(_args) do
    for {table, opts} <- @tables_opt do
      :ets.new(table, opts)
    end

    load_all()

    :ignore
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
    for table <- @tables do
      :ets.delete(table)
    end
  end

  def terminate do
    save_all()
    delete_all()
    :persistent_term.erase(:save_dir)
    :ok
  end
end
