import Config

# prod = config_env() == :prod

config :ipncore, :central, "ippan.net"
config :ipncore, :channel, "BETA-NET"
config :ipncore, :gps_device, "/dev/AMC0"

# environment variables
data_dir = System.get_env("DATA_DIR", "data")
cert_dir = System.get_env("CERT_DIR", "priv/cert")

# folder paths
config :ipncore, :data_path, data_dir
config :ipncore, :wallet_path, Path.join(data_dir, "wallets")
config :ipncore, :balance_path, Path.join(data_dir, "balances")
config :ipncore, :events_path, Path.join(data_dir, "events")
config :ipncore, :post_path, Path.join(data_dir, "posts")

# DNS config
config :ipncore, :dns,
  ip: '0.0.0.0',
  port: 53

config :ipncore, :dns6,
  ip: '::',
  port: 53

config :ipncore, :dns_tls,
  ip: {0, 0, 0, 0},
  port: 853,
  transport_options: [
    certfile: Path.join(cert_dir, "cert.pem"),
    cacertfile: Path.join(cert_dir, "cacert.pem"),
    keyfile: Path.join(cert_dir, "key.pem"),
    send_timeout: 15_000
  ],
  num_acceptors: 20,
  read_timeout: :infinity

# IMP config
# config :ipncore, :imp_client,
#   host: "us2.ippan.net",
#   port: 8484,
#   cert_dir: cert_dir,
#   node_type: 0,
#   role: :core

# HTTP config
config :ipncore, :http,
  host: "0.0.0.0",
  port: 80,
  acceptors: 100,
  max_conn: 16384

config :ipncore, :https,
  host: "0.0.0.0",
  port: 443,
  cert_dir: cert_dir,
  acceptors: 100,
  max_conn: 16384

# database
config :ipncore, Ipncore.Repo,
  hostname: "localhost",
  username: "kambei",
  database: "ippan",
  password: "NdgPPUWiSXF1EQbC5Pqm",
  port: 5432,
  pool_size: 20,
  show_sensitive_data_on_connection_error: true,
  ssl: false,
  ssl_opts: [
    cacertfile: Path.join(cert_dir, "cacert.pem"),
    certfile: Path.join(cert_dir, "cert.pem"),
    keyfile: Path.join(cert_dir, "key.pem")
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

config :ipncore, :dns_resolve_opts,
  alt_nameservers: [
    {{8, 8, 8, 8}, 53},
    {{1, 0, 0, 1}, 53},
    {{208, 67, 220, 220}, 53}
  ],
  nameservers: [
    {{1, 1, 1, 1}, 53},
    {{8, 8, 4, 4}, 53},
    {{9, 9, 9, 9}, 53},
    {{208, 67, 222, 222}, 53}
  ],
  timeout: 5_000

# deliver max file size
config :ipncore, :max_file_size, 1_000_000_000
