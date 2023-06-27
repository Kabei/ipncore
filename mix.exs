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
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      # compilers: [:elixir_make] ++ Mix.compilers(),
      deps: deps(),
      package: package(),
      # Release
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
      # {:zigler, "~> 0.9.1", runtime: false},
      {:benchee, "~> 1.0", only: [:dev, :test]},
      {:bakeware, "~> 0.2.4", runtime: false},
      {:globalconst, "~> 0.3.2"},
      {:poolboy, "~> 1.5.2"},
      {:dnslib, git: "https://github.com/lateio/dnslib", branch: "master", override: true},
      # {:socket, "~> 0.3"},
      {:sntp, "~> 0.2.0"},
      {:jason, "~> 1.4"},
      # {:uuidv4, "1.0.0"},
      # {:jsonrs, "~> 0.3.0"},
      # {:ecto_sql, "~> 3.8"},
      # {:postgrex, ">= 0.0.0"},
      # {:plug_cowboy, "~> 2.0"},
      {:bandit, ">= 0.7.7"},
      {:libdecaf, "~> 2.1.1"},
      {:phoenix_pubsub_redis, "~> 3.0.1"},
      # {:download, "~> 0.0.0"},
      # {:dets_plus, path: "../dets_plus"},
      # {:thousand_island, "~> 0.6.4"},
      # {:phoenix_pubsub, "~> 2.0"},
      {:blake3, path: "../blake3"},
      # {:blake3, "~> 1.0"},
      # {:jsonrs, "~> 0.3.0"},
      # {:curvy, "~> 0.3"},
      {:ex_secp256k1, "~> 0.7"},
      {:ed25519_blake2b, path: "../ed25519_blake2b"},
      {:cafezinho, "~> 0.4.0"},
      # local deps
      {:exqlite, path: "../exqlite"},
      {:falcon, path: "../falcon"},
      {:ntrukem, path: "../ntrukem"},
      {:fast64, path: "../fast64_elixir"}
      # {:ipnutils, path: "../ipnutils"}
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
end
