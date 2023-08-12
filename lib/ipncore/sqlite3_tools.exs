defmodule Sqlite3Tools do
  @url (case :os.type() do
          {:unix, :linux} ->
            "https://www.sqlite.org/2023/sqlite-tools-linux-x86-3420000.zip"

          {:unix, :darwin} ->
            "https://www.sqlite.org/2023/sqlite-tools-osx-x86-3420000.zip"

          _ ->
            raise CompileError, "OS not supported"
        end)

  @hash (case :os.type() do
           {:unix, :linux} ->
             <<169, 220, 88, 4, 203, 97, 239, 199, 58, 102, 170, 97, 176, 52, 88, 146, 22, 222,
               159, 126, 83, 170, 52, 166, 77, 210, 38, 141, 142, 133, 191, 219>>

           {:unix, :darwin} ->
             <<135, 164, 221, 54, 195, 120, 26, 19, 24, 22, 87, 206, 232, 91, 240, 227, 53, 85,
               77, 227, 216, 158, 125, 187, 227, 6, 196, 169, 178, 146, 245, 209>>

           _ ->
             raise CompileError, "OS not supported"
         end)

  @priv_dir :code.priv_dir(:ipncore) |> to_string()
  @dirname @url |> Path.basename(".zip")
  @workdir Path.join(@priv_dir, @dirname)
  @executable Path.join(@workdir, "sqlite3")
  @filepath Path.join([@workdir, "sqlite3"])
  @hash_type :sha3_256

  def init do
    unless File.exists?(@filepath) do
      zip_file = Path.join(@priv_dir, Path.basename(@url))

      :ok = Download.from(@url, zip_file)

      if hash_file(zip_file) != @hash do
        raise RuntimeError, "Sqlite3 Zipfile hash not match"
      end

      {_, 0} = System.cmd("unzip", ["-q", zip_file], cd: @priv_dir)
      File.rm(zip_file)
    end
  end

  def backup(dbpath, output) do
    System.cmd(@executable, [dbpath, ".backup", output])
  end

  defp hash_file(path) do
    state = :crypto.hash_init(@hash_type)

    File.stream!(path, [], 2048)
    |> Enum.reduce(state, &:crypto.hash_update(&2, &1))
    |> :crypto.hash_final()
  end
end
