defmodule Ipncore.MixProject do
  use Mix.Project

  @app :ipncore
  @version "0.4.0"
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
      steps: [:assemble, &Bakeware.assemble/1],
      strip_beams: Mix.env() == :prod
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    if System.otp_release() |> String.to_integer() < @min_otp,
      do: raise(RuntimeError, "OTP invalid version. Required minimum v#{@min_otp}")

    # load env file
    System.get_env("ENV_FILE", "env_file") |> load_ini_file()

    [
      extra_applications: [:crypto, :syntax_tools, :logger],
      mod: {Ipncore.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:bakeware, "~> 0.2.4", runtime: false},
      {:globalconst, "~> 0.3.2"},
      {:poolboy, "~> 1.5.2"},
      {:dnslib, git: "https://github.com/lateio/dnslib", branch: "master", override: true},
      {:sntp, "~> 0.2.0"},
      {:jason, "~> 1.4"},
      {:msgpack, "~> 0.8.1"},
      {:bandit, ">= 0.7.7"},
      {:phoenix_pubsub_redis, "~> 3.0.1"},
      # {:download, "~> 0.0.4"},
      {:cafezinho, "~> 0.4.0"},
      {:httpoison, "~> 2.0"},
      {:blake3, git: "https://kabei@github.com/kabei/blake3.git", branch: "master"},
      {:exqlite, git: "https://kabei@github.com/kabei/exqlite.git", branch: "main"},
      {:falcon, git: "https://kabei@github.com/kabei/falcon.git", branch: "master"},
      {:ntrukem, git: "https://kabei@github.com/kabei/ntrukem.git", branch: "master"},
      {:fast64, git: "https://kabei@github.com/kabei/fast64_elixir.git", branch: "master"}
    ]
  end

  def package do
    [
      name: @app,
      description: "IPPAN Core",
      maintainers: ["Kambei Sapote"],
      licenses: ["MIT"],
      files: ["lib/*", "mix.exs", "README*", "LICENSE*"]
    ]
  end

  defp load_ini_file(path) do
    if File.exists?(path) do
      File.stream!(path, [], :line)
      |> Enum.each(fn text ->
        text
        |> String.trim()
        |> String.replace(~r/\n|\r|#.+/, "")
        |> String.split("=", parts: 2)
        |> case do
          [key, value] ->
            System.put_env(key, value)

          _ ->
            :ignored
        end
      end)
    end
  end
end
