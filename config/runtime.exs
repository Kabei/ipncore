import Config

# prod = config_env() == :prod

config :ipncore, :central, "ippan.net"
config :ipncore, :channel, "DEV-NET"
config :ipncore, :gps_device, "/dev/AMC0"

# folder paths
config :ipncore, :data_path, "data"
config :ipncore, :wallet_path, "data/wallets"
config :ipncore, :balance_path, "data/balances"
config :ipncore, :events_path, "data/events"

# DNS config
config :ipncore, :dns_port, 53

config :ipncore, :imp_client,
  host: "us2.ippan.net",
  port: 8484,
  cert_dir: "priv/cert",
  node_type: 0

config :ipncore, :http,
  host: "0.0.0.0",
  port: 80,
  acceptors: 100,
  max_conn: 16384

config :ipncore, :https,
  host: "0.0.0.0",
  port: 443,
  cert_dir: "priv/cert",
  acceptors: 100,
  max_conn: 16384

# database
config :ipncore, Ipncore.Repo,
  hostname: "core.ippan.net",
  username: "ippan",
  database: "ippancore_beta",
  password: "NdgPPUWiSXF1EQbC5Pqm",
  port: 5432,
  pool_size: 20,
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

config :ipncore, :ntp_servers, [
  '0.north-america.pool.ntp.org',
  '1.north-america.pool.ntp.org',
  '2.north-america.pool.ntp.org',
  '0.europe.pool.ntp.org',
  '1.europe.pool.ntp.org',
  '2.europe.pool.ntp.org',
  '0.asia.pool.ntp.org',
  '1.asia.pool.ntp.org',
  '2.asia.pool.ntp.org',
  '0.oceania.pool.ntp.org',
  '0.africa.pool.ntp.org',
  'hora.roa.es',
  'time.google.com',
  'time.cloudflare.com',
  'time.windows.com'
]

# deliver max file size
config :ipncore, :max_file_size, 1_000_000_000

config :ipncore, :post_path, System.get_env("POSTS_PATH") || "~/posts"
