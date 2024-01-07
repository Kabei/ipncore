import Config

# Number of cores available
cpus = System.schedulers_online()

# Environment variables setup
port = System.get_env("PORT", "5815") |> String.to_integer()
cluster_port = System.get_env("CLUSTER_PORT", "4848") |> String.to_integer()
http_port = System.get_env("HTTP_PORT", "8080") |> String.to_integer()
x_http_port = System.get_env("X_HTTP_PORT", "8080") |> String.to_integer()

# Network setup
# P2P server
config :ipncore, :network,
  handler_module: Ippan.NetworkServer,
  transport_module: ThousandIsland.Transports.TCP,
  num_acceptors: 100,
  port: port,
  transport_options: [
    backlog: 1024,
    nodelay: true,
    linger: {true, 30},
    send_timeout: 30_000,
    send_timeout_close: true,
    reuseaddr: true,
    packet: 4,
    packet_size: 1_000_000
  ]

# Cluster setup
config :ipncore, :cluster,
  handler_module: Ippan.ClusterServer,
  transport_module: ThousandIsland.Transports.TCP,
  num_acceptors: 100,
  port: cluster_port,
  transport_options: [
    backlog: 1024,
    nodelay: true,
    linger: {true, 30},
    send_timeout: 30_000,
    send_timeout_close: true,
    reuseaddr: true,
    packet: 4,
    packet_size: 1_000_000
  ]

# HTTP server
config :ipncore, :http,
  plug: Ipncore.Endpoint,
  scheme: :http,
  port: http_port,
  http_1_options: [
    compress: false
  ],
  thousand_island_options: [
    num_acceptors: 100,
    read_timeout: 60_000,
    num_connections: 16_384,
    max_connections_retry_count: 5,
    max_connections_retry_wait: 1000,
    shutdown_timeout: 60_000,
    transport_options: [
      backlog: 1024,
      nodelay: true,
      linger: {true, 30},
      send_timeout: 20_000,
      send_timeout_close: true,
      reuseaddr: true
    ]
  ]

  config :ipncore, :x_http_port, x_http_port
