defmodule Ippan.Funx.Sys do
  @app Mix.Project.config()[:app] |> to_string()

  def upgrade(_, %{} = %{"git" => git} = opts, target) do
    contains_app = Enum.any?(target, fn x -> x == @app end)

    if contains_app do
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

                "get & clean" ->
                  System.cmd("mix", ["deps.get"])
                  System.cmd("mix", ["deps.clean", "--unlock", "--unused"])
              end

              # compile
              case Map.get(opts, "compile") do
                "force" ->
                  System.cmd("mix", ["compile", "--force"])

                _ ->
                  System.cmd("mix", ["compile"])
              end

            _ ->
              :error
          end
        end)

      # reset
      case Map.get(opts, "reset") do
        "all" ->
          fun = fn ->
            case Task.await(task, :infinity) do
              :error -> nil
              _ -> System.restart()
            end
          end

          :persistent_term.put(:last_fun, fun)

        nil ->
          :ok
      end
    else
      :ok
    end
  end
end
