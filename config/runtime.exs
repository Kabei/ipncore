import Config

# prod = config_env() == :prod

config :ipncore, :channel, "IPN-003"
config :ipncore, :central, "ippan.net"

config :ipncore, :imp_client,
  host: "pool.ippan.net",
  port: 8484,
  falcon_file: "priv/cert/falcon.keys",
  kem_file: "priv/cert/falcon.keys",
  node_type: 0

# database
config :ipncore, Ipncore.Repo,
  hostname: "127.0.0.1",
  username: "ippan",
  database: "ippancore",
  password: "NdgPPUWiSXF1EQbC5Pqm",
  port: 5432,
  pool_size: 10,
  show_sensitive_data_on_connection_error: true,
  ssl: false,
  ssl_opts: [
    cacertfile: "priv/cert/db/cacert.pem",
    certfile: "priv/cert/db/cert.pem",
    keyfile: "priv/cert/db/key.pem"
  ],
  prepare: :unnamed,
  timeout: 30_000,
  queue_interval: 2_000,
  queue_target: 5_000

# tmp, fs, ram, tmpfs
config :ipncore, :cubdb,
  check_expiry: 300_000,
  blocks: [
    %{
      type: :fs,
      root: "priv/buckets",
      buckets: [
        "txpool"
      ],
      compress: false
    }
  ]

# deliver max file size
config :ipncore, :max_file_size, 1_000_000_000

config :ipncore, :post_path, System.get_env("POSTS_PATH") || "~/posts"
