defmodule Ipncore.MixProject do
  use Mix.Project

  def project do
    [
      app: :ipncore,
      version: "0.1.0",
      config_path: "config/config.exs",
      elixir: "~> 1.14",
      otp: "~> 24.0",
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
      extra_applications: [:crypto, :syntax_tools, :logger, :download],
      mod: {Ipncore.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:xxhash, "~> 0.3.1"},
      # {:dns, "~> 2.4.0"},
      # {:socket, "~> 0.3.13"},
      # {:httpoison, "~> 1.7"},
      # {:cbor, "~> 1.0.0"},
      {:dnslib, git: "https://github.com/lateio/dnslib", branch: "master", override: true},
      {:socket, "~> 0.3"},
      {:sntp, "~> 0.2.0"},
      {:jason, "~> 1.4"},
      {:ecto_sql, "~> 3.8"},
      {:postgrex, ">= 0.0.0"},
      {:plug_cowboy, "~> 2.0"},
      {:benchee, "~> 1.0", only: :dev},
      {:download, "~> 0.0.0"},
      {:dets_plus, "~> 2.1"},
      {:thousand_island, "~> 0.5.15"},
      # local deps
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
