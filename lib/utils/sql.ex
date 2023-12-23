defmodule SQL do
  def readFile!(filename, args \\ %{}) do
    File.read!(filename)
    |> String.trim()
    |> put_args(args)
    |> String.split(~r/;\s\s/, trim: true)
    |> Enum.filter(fn txt ->
      String.trim(txt) |> String.starts_with?("--") |> Kernel.not()
    end)
  end

  def readStmtFile!(filename, args \\ %{}) do
    data =
      File.read!(filename)
      |> String.trim()
      |> put_args(args)
      |> String.split("\n", trim: true)

    keys =
      data
      |> Enum.filter(fn txt -> String.trim(txt) |> String.starts_with?("--name:") end)
      |> Enum.map(fn txt -> txt |> String.replace("--name:", "") |> String.trim() end)

    values =
      data
      |> Enum.filter(fn txt ->
        String.trim(txt) |> String.starts_with?("--") |> Kernel.not()
      end)
      |> Enum.join()
      |> String.split(~r/;/, trim: true)
      |> Enum.map(fn x -> String.to_charlist(x) end)

    Enum.zip([keys, values])
    |> Map.new()
  end

  defp put_args(txt, %{}), do: txt

  defp put_args(txt, args) do
    args
    |> Enum.reduce(txt, fn {pattern, replacement}, acc ->
      regex = Regex.compile!("\\$" <> pattern <> "\\b", "")
      String.replace(acc, regex, replacement)
    end)
  end
end
