defmodule Ipncore.MixProject do
  use Mix.Project

  @min_otp 25

  def project do
    [
      app: :ipncore,
      version: "0.1.0",
      config_path: "config/config.exs",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      # compilers: [:elixir_make] ++ Mix.compilers(),
      deps: deps(),
      package: package(),
      # Release
      releases: [
        ipncore: [
          include_executables_for: [:unix],
          applications: [runtime_tools: :permanent]
        ]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    if System.otp_release() |> String.to_integer() < @min_otp, do: raise("OTP invalid version")

    [
      extra_applications: [:crypto, :syntax_tools, :logger],
      mod: {Ipncore.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:elixir_make, "~> 0.4", runtime: false},
      # {:xxhash, "~> 0.3.1"},
      # {:dns, "~> 2.4.0"},
      # {:cbor, "~> 1.0.0"},
      {:globalconst, "~> 0.3.2"},
      {:poolboy, "~> 1.5.2"},
      {:benchee, "~> 1.0", only: [:test]},
      {:dnslib, git: "https://github.com/lateio/dnslib", branch: "master", override: true},
      # {:socket, "~> 0.3"},
      {:sntp, "~> 0.2.0"},
      {:jason, "~> 1.4"},
      # {:ecto_sql, "~> 3.8"},
      # {:postgrex, ">= 0.0.0"},
      # {:plug_cowboy, "~> 2.0"},
      # {:bandit, ">= 0.6.10"},
      # {:download, "~> 0.0.0"},
      # {:dets_plus, path: "../dets_plus"},
      {:thousand_island, "~> 0.6.4"},
      {:phoenix_pubsub, "~> 2.0"},
      {:blake3, "~> 1.0"},
      # local deps
      {:exqlite, path: "../exqlite"},
      {:falcon, path: "../falcon"},
      {:ntrukem, path: "../ntrukem"}
      # {:ipnutils, path: "../ipnutils"}
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
