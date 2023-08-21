import Config

# Number of cores available
cpus = System.schedulers_online()

# Environment variables setup
port = System.get_env("PORT", "5815") |> String.to_integer()
http_port = System.get_env("HTTP_PORT", "8080") |> String.to_integer()
data_dir = System.get_env("DATA_DIR", "data")
kem_dir = System.get_env("KEM_DIR", "priv/kem.key")
falcon_dir = System.get_env("FALCON_DIR", "priv/falcon.key")
key_dir = System.get_env("KEY_DIR", "priv/secret.key")
role = System.get_env("ROLE", "verifier")
is_miner = role == "miner"

# Folders setup
config :ipncore, :data_dir, data_dir

# Folder cert
config :ipncore, :kem_dir, kem_dir
config :ipncore, :falcon_dir, falcon_dir
config :ipncore, :key_dir, key_dir

# Node setup
config :ipncore, :role, role
config :ipncore, :vid, System.get_env("VID", "0") |> String.to_integer()

# Network setup
{num_acceptors, num_connections} =
  if is_miner do
    {max(cpus, 10), 4096}
  else
    {100, 16_384}
  end

# P2P server
config :ipncore, :P2P,
  handler_module: Ippan.P2P.Server,
  transport_module: ThousandIsland.Transports.TCP,
  num_acceptors: max(cpus, 10),
  port: port,
  transport_options: [
    backlog: 1024,
    nodelay: true,
    linger: {true, 30},
    send_timeout: 30_000,
    send_timeout_close: true,
    reuseaddr: true,
    packet: 2,
    packet_size: 9_000
  ]

# HTTP server
config :ipncore, :http,
  port: http_port,
  http_1_options: [
    compress: false
  ],
  thousand_island_options: [
    num_acceptors: 10,
    read_timeout: 60_000,
    num_connections: 4096,
    max_connections_retry_count: 5,
    max_connections_retry_wait: 1000,
    shutdown_timeout: 60_000,
    transport_options: [
      backlog: 1024,
      nodelay: true,
      linger: {true, 30},
      send_timeout: 10_000,
      send_timeout_close: true,
      reuseaddr: true
    ]
  ]

# NTP servers
config :ipncore, :ntp_servers, [
  ~c"0.north-america.pool.ntp.org",
  ~c"1.north-america.pool.ntp.org",
  ~c"2.north-america.pool.ntp.org",
  ~c"0.europe.pool.ntp.org",
  ~c"1.europe.pool.ntp.org",
  ~c"2.europe.pool.ntp.org",
  ~c"0.asia.pool.ntp.org",
  ~c"1.asia.pool.ntp.org",
  ~c"2.asia.pool.ntp.org",
  ~c"0.oceania.pool.ntp.org",
  ~c"0.africa.pool.ntp.org",
  ~c"hora.roa.es",
  ~c"time.google.com",
  ~c"time.cloudflare.com",
  ~c"time.windows.com"
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
