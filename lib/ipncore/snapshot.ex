defmodule Snapshot do
  require Sqlite
  alias Ippan.DetsSup
  @app Mix.Project.config()[:app]
  @extension Application.compile_env(@app, :snap_extension, "snap")

  @key "last_snap"
  @default %{hash: nil, id: -1, size: 0}

  def create(round_id) do
    before_create()
    filename = String.to_charlist("#{round_id}.#{@extension}")

    files =
      :persistent_term.get(:store_dir)
      |> File.ls!()
      |> Enum.map(fn x -> String.to_charlist(x) end)

    to = :persistent_term.get(:save_dir)

    # compress folder
    :zip.create(filename, files, cwd: to)

    filepath = Path.join(to, filename)

    # compute filehash
    hash =
      compute_hashfile(filepath)

    # get filesize
    {:ok, fstat} = File.stat(filepath)

    # save record
    stats = Stats.new()
    snapshot = %{"id" => round_id, "hash" => hash, "size" => fstat.size}
    Stats.put(stats, @key, snapshot)
    DetsPlux.start_sync(stats.db, stats.tx)
  end

  def last do
    stats = Stats.cache()

    Stats.get(stats, @key, nil)
    |> to_map()
  end

  def last(stats) do
    Stats.get(stats, @key, nil)
    |> to_map()
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

  # download and hash verification snapshot file
  def download(hostname, snapshot) do
    url = "https://#{hostname}/v1/dl/save/#{snapshot.id}"

    filepath = :persistent_term.get(:save_dir) |> Path.join("#{snapshot.id}.#{@extension}")
    DownloadTask.start(url, filepath, snapshot.size)

    # snapshot hash verification
    case compute_hashfile(filepath) == snapshot.hash do
      true -> :ok
      _ -> :error
    end
  end

  def local_download(hostname, snapshot) do
    port = Application.get_env(@app, :x_http_port)
    url = "http://#{hostname}:#{port}/v1/dl/save/#{snapshot.id}"

    filepath = :persistent_term.get(:save_dir) |> Path.join("#{snapshot.id}.#{@extension}")
    DownloadTask.start(url, filepath, snapshot.size)

    # snapshot hash verification
    case compute_hashfile(filepath) == snapshot.hash do
      true -> :ok
      _ -> :error
    end
  end

  def to_map(%{"id" => id, "hash" => hash, "size" => size}) do
    %{
      id: id,
      hash: hash,
      size: size
    }
  end

  def to_map(_), do: @default

  # delete old rounds and blocks records
  defp before_create do
    db_ref = :persistent_term.get(:main_conn)
    Sqlite.step("delete_old_blocks")
    Sqlite.step("delete_old_rounds")
    Sqlite.sync(db_ref)
  end

  # stop database processes
  defp before_restore do
    :persistent_term.put(:status, :sync)
    MainStore.terminate()
    Supervisor.stop(DetsSup)
  end

  # reset system
  defp after_restore do
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
