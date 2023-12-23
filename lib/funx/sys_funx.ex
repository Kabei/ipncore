defmodule Ippan.Funx.Sys do
  @app Mix.Project.config()[:app] |> to_string()

  def upgrade(_, %{} = %{"git" => git} = opts, target) do
    if @app in target do
      # run commands
      task =
        Task.async(fn ->
          result =
            if is_list(git) do
              for cmd <- git do
                args = String.split(cmd, " ", trim: true)
                System.cmd("git", args)
              end
            else
              args = String.split(git, " ", trim: true)
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

      Task.await(task, :infinity)

      # reset
      case Map.get(opts, "reset") do
        "all" ->
          fun = fn ->
            IO.puts("Restart")
            System.restart()
          end

          :persistent_term.put(:last_fun, fun)

        _ ->
          :ok
      end
    end
  end
end
