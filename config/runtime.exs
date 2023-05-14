import Config

# prod = config_env() == :prod

config :ipncore, :central, "ippan.com"
config :ipncore, :channel, "BETA-NET"
config :ipncore, :gps_device, "/dev/AMC0"

# environment variables
hostname = System.get_env("HOSTNAME", "ippan.uk")
data_dir = System.get_env("DATA_DIR", "data")
# ssl certificates
cert_dir = System.get_env("CERT_DIR", "priv/cert/cert.pem")
key_dir = System.get_env("KEY_DIR", "priv/cert/key.pem")
cacert_dir = System.get_env("CACERT_DIR", "priv/cert/cacert.pem")
falcon_dir = System.get_env("FALCON_DIR", "priv/cert/falcon.keys")

# folder paths
config :ipncore, :data_dir, data_dir
config :ipncore, :wallet_path, Path.join(data_dir, "wallets")
config :ipncore, :balance_path, Path.join(data_dir, "balances")
config :ipncore, :events_path, Path.join(data_dir, "events")
config :ipncore, :post_path, Path.join(data_dir, "posts")

config :ipncore, :falcon_dir, falcon_dir

# DNS config
config :ipncore, :dns,
  ip: {0, 0, 0, 0},
  port: 53

config :ipncore, :dns6,
  ip: {0, 0, 0, 0, 0, 0, 0, 0},
  port: 53

config :ipncore, :dns_tls,
  handler_module: Ipncore.DNS.TlsServer,
  transport_module: ThousandIsland.Transports.SSL,
  port: 853,
  num_acceptors: 10,
  read_timeout: 120_000,
  transport_options: [
    cacertfile: cacert_dir,
    certfile: cert_dir,
    keyfile: key_dir,
    send_timeout: 15_000
  ]

config :ipncore, :node,
  name: hostname,
  port: 5050

# HTTP config
config :ipncore, :http,
  port: 80,
  thousand_island_options: [
    num_acceptors: 10,
    read_timeout: 15_000,
    transport_options: [
      backlog: 1024,
      nodelay: true,
      linger: {true, 30},
      send_timeout: 15_000,
      send_timeout_close: true,
      reuseaddr: true
    ]
  ]

config :ipncore, :https,
  port: 443,
  thousand_island_options: [
    num_acceptors: 10,
    read_timeout: 15_000,
    transport_options: [
      cacertfile: cacert_dir,
      certfile: cert_dir,
      keyfile: key_dir,
      backlog: 1024,
      nodelay: true,
      linger: {true, 30},
      send_timeout: 15_000,
      send_timeout_close: true,
      reuseaddr: true
    ]
  ]

# database config
config :ipncore, Ipncore.Repo,
  hostname: "ippan.uk",
  username: "kambei",
  database: "ippan",
  password: "NdgPPUWiSXF1EQbC5Pqm",
  port: 5432,
  pool_size: 20,
  show_sensitive_data_on_connection_error: true,
  ssl: false,
  ssl_opts: [
    cacertfile: cacert_dir,
    certfile: cert_dir,
    keyfile: key_dir
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

config :ipncore, :dns_resolve,
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
