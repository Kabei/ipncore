defmodule Snapshot do
  require Sqlite
  alias Ippan.DetsSup
  @app Mix.Project.config()[:app]
  @extension Application.compile_env(@app, :snap_extension, "snap")

  def create(round_id) do
    before_create()
    filename = String.to_charlist("#{round_id}.#{@extension}")

    files =
      :persistent_term.get(:store_dir)
      |> File.ls!()
      |> Enum.map(fn x -> String.to_charlist(x) end)

    to = :persistent_term.get(:save_dir)

    :zip.create(filename, files, cwd: to)

    hash =
      Path.join(to, filename)
      |> compute_hashfile()

    stats = Stats.new()
    Stats.put(stats, "last_snap", round_id)
    Stats.put(stats, "last_snap_hash", hash)
    DetsPlux.start_sync(stats.db, stats.tx)
  end

  def restore(round_id) do
    before_restore()
    cwd = :persistent_term.get(:store_dir)

    file =
      Path.join(:persistent_term.get(:save_dir), "#{round_id}.#{@extension}")
      |> String.to_charlist()

    :zip.extract(file, cwd: cwd)
    after_restore()
  end

  defp before_create do
    db_ref = :persistent_term.get(:main_conn)
    Sqlite.step("delete_old_blocks")
    Sqlite.step("delete_old_rounds")
    Sqlite.sync(db_ref)
  end

  defp before_restore do
    :persistent_term.put(:status, :sync)
    MainStore.terminate()
    Supervisor.stop(DetsSup)
  end

  defp after_restore do
    MainStore.init(nil)
    DetsSup.init(nil)
    :init.restart()
  end

  @hash_module Blake3
  defp compute_hashfile(path) do
    state = @hash_module.new()

    File.stream!(path, [], 2048)
    |> Enum.reduce(state, &@hash_module.update(&2, &1))
    |> @hash_module.finalize()
  end
end
