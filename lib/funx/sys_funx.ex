defmodule Ippan.Funx.Sys do
  @app Mix.Project.config()[:app] |> to_string()

  def upgrade(_, %{"git" => git} = opts, target) do
    if @app in target do
      # run commands
      pid =
        spawn(fn ->
          result =
            if is_list(git) do
              for cmd <- git do
                args = String.split(cmd, " ", trim: true)
                System.cmd("git", args ++ ["--quiet"])
              end
            else
              args = String.split(git, " ", trim: true) ++ ["--quiet"]
              System.cmd("git", args)
            end

          case result do
            {_, 0} ->
              # get deps
              case Map.get(opts, "deps") do
                "get" ->
                  System.cmd("mix", ["deps.get"])

                "unused" ->
                  System.cmd("mix", ["deps.clean", "--unlock", "--unused"])

                "get_and_unused" ->
                  System.cmd("mix", ["deps.clean", "--unlock", "--unused"])
                  System.cmd("mix", ["deps.get"])

                nil ->
                  :ok
              end

              # compile
              case Map.get(opts, "compile") do
                "force" ->
                  System.cmd("mix", ["compile", "--force"])

                "normal" ->
                  System.cmd("mix", ["compile"])

                _ ->
                  :ok
              end

            _ ->
              :error
          end
        end)

      fun = fn ->
        after_upgrade(opts, pid)
      end

      :persistent_term.put(:last_fun, fun)
    end
  end

  defp after_upgrade(opts, pid) do
    Process.monitor(pid)

    receive do
      {:DOWN, _ref, _process, _pid, _normal} ->
        case Map.get(opts, "reset") do
          # reset all
          "all" ->
            IO.puts("Restart")
            System.restart()

          # reset specific modules
          files when is_list(files) ->
            :code.all_loaded()
            |> Enum.each(fn {mod, path} ->
              if Path.basename(path, ~c".beam") in files do
                :code.soft_purge(mod)
                :code.load_file(mod)
              end
            end)

          _ ->
            :ok
        end
    end
  end
end
