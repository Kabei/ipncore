defmodule Ipncore.MixProject do
  use Mix.Project

  def project do
    [
      app: :ipncore,
      version: "0.1.0",
      config_path: "config/config.exs",
      elixir: "~> 1.12",
      otp: "~> 23.0",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      package: package(),
      # Release
      releases: [
        ipncore: [
          include_executables_for: [:unix],
          applications: [runtime_tools: :permanent]
          # config_providers: [
          #   {JSONConfigProvider, "./config.json"}
          # ]
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:crypto, :socket, :syntax_tools, :logger, :download],
      mod: {Ipncore.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:xxhash, "~> 0.3.1"},
      {:cbor, "~> 1.0.0"},
      {:dns, "~> 2.4.0"},
      {:sntp, "~> 0.2.0"},
      {:jason, "~> 1.4"},
      {:ecto_sql, "~> 3.8"},
      {:postgrex, ">= 0.0.0"},
      {:plug_cowboy, "~> 2.0"},
      {:benchee, "~> 1.0", only: :dev},
      {:download, "~> 0.0.0"},
      {:httpoison, "~> 1.7"},
      {:dets_plus, "~> 2.1"},

      {:ntrukem, path: "../ntrukem"},
      {:falcon, path: "../falcon"},
      {:ipnutils, path: "../ipnutils"},
      {:imp, path: "../imp"}
    ]
  end

  def package do
    [
      name: :ipncore,
      description: "IPPAN Core",
      maintainers: ["Kambei Sapote"],
      licenses: ["MIT"],
      files: ["lib/*", "mix.exs", "README*", "LICENSE*"]
    ]
  end
end
