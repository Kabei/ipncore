defmodule Ipncore.MixProject do
  use Mix.Project

  @app :ipncore
  @version "0.5.0"
  @min_otp 25

  def project do
    [
      app: @app,
      version: @version,
      config_path: "config/config.exs",
      elixir: "~> 1.14",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      releases: [
        {@app, release()},
        unix: [
          include_executables_for: [:unix],
          applications: [runtime_tools: :permanent]
        ]
      ]
    ]
  end

  defp release do
    [
      overwrite: true,
      quiet: true,
      # steps: [:assemble, &Bakeware.assemble/1],
      strip_beams: Mix.env() == :prod
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    if System.otp_release() |> String.to_integer() < @min_otp,
      do: raise(RuntimeError, "OTP invalid version. Required minimum v#{@min_otp}")

    # load env file
    System.get_env("ENV_FILE", "env_file") |> load_env_file()

    [
      extra_applications: [:crypto, :syntax_tools, :logger],
      mod: {Ipncore.Application, []}
    ]
  end

  defp load_env_file(path) do
    File.stream!(path, [], :line)
    |> Enum.each(fn text ->
      text
      |> String.trim()
      |> String.replace(~r/\n|\r|#.+/, "")
      |> String.split("=", parts: 2, trim: true)
      |> case do
        [key, value] ->
          System.put_env(key, value)

        _ ->
          :ignored
      end
    end)
  end

  def package do
    [
      name: @app,
      maintainers: ["Kambei Sapote"],
      licenses: ["MIT"],
      files: ["lib/*", "mix.exs", "README*", "LICENSE*"]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:poolboy, "~> 1.5.2"},
      {:jason, "~> 1.4"},
      {:bandit, ">= 0.7.7"},
      {:phoenix_pubsub, "~> 2.1"},
      {:httpoison, "~> 2.0"},
      {:cafezinho, "~> 0.4.0"},
      {:blake3, git: "https://kabei@github.com/kabei/blake3.git", branch: "master"},
      {:exqlite, git: "https://kabei@github.com/kabei/exqlite.git", branch: "main"},
      {:falcon, git: "https://kabei@github.com/kabei/falcon.git", branch: "master"},
      {:ntrukem, git: "https://kabei@github.com/kabei/ntrukem.git", branch: "master"},
      {:fast64, git: "https://kabei@github.com/kabei/fast64_elixir.git", branch: "master"}
    ]
  end
end
