import Config

# config :ipncore, :gps_device, "/dev/AMC0"
# ssl certificates
# cert_dir = System.get_env("CERT_DIR", "priv/cert/cert.pem")
# key_dir = System.get_env("KEY_DIR", "priv/cert/key.pem")
# cacert_dir = System.get_env("CACERT_DIR", "priv/cert/cacert.pem")

# environment variables
port = System.get_env("PORT", "5815") |> String.to_integer()
http_port = System.get_env("HTTP_PORT", "8080") |> String.to_integer()

data_dir = System.get_env("DATA_DIR", "data")
kem_dir = System.get_env("KEM_DIR", "priv/kem.key")
falcon_dir = System.get_env("FALCON_DIR", "priv/falcon.key")

# folder paths
config :ipncore, :data_dir, data_dir

# folder cert
config :ipncore, :kem_dir, kem_dir
config :ipncore, :falcon_dir, falcon_dir

# node
config :ipncore, :role, System.get_env("ROLE", "verifier")
config :ipncore, :vid, System.get_env("VID", "0") |> String.to_integer()
config :ipncore, :hostname, System.get_env("HOSTNAME", "localhost")
config :ipncore, :redis, System.get_env("REDIS")

# p2p server
config :ipncore, :p2p,
  handler_module: Ippan.P2P.Server,
  transport_module: ThousandIsland.Transports.TCP,
  port: port,
  num_acceptors: 10

# http server
config :ipncore, :http,
  port: http_port,
  http_1_options: [
    compress: false
  ],
  thousand_island_options: [
    num_acceptors: 500,
    read_timeout: 60_000,
    num_connections: 16_384,
    max_connections_retry_count: 5,
    max_connections_retry_wait: 1000,
    shutdown_timeout: 60_000,
    transport_options: [
      backlog: 1024,
      nodelay: true,
      linger: {true, 30},
      send_timeout: 15_000,
      send_timeout_close: true,
      reuseaddr: true
    ]
  ]

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
