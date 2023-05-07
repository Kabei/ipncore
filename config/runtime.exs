import Config

# config :ipncore, :chain, "BETA-NET"
# config :ipncore, :gps_device, "/dev/AMC0"

# environment variables
hostname = System.get_env("HOSTNAME", "ippan.uk")
id = System.get_env("NODE_ID", "0") |> String.to_integer()

data_dir = System.get_env("DATA_DIR", "data")
kem_dir = System.get_env("KEM_DIR", "priv/kem.key")
falcon_dir = System.get_env("FALCON_DIR", "priv/falcon.key")
# ssl certificates
# cert_dir = System.get_env("CERT_DIR", "priv/cert/cert.pem")
# key_dir = System.get_env("KEY_DIR", "priv/cert/key.pem")
# cacert_dir = System.get_env("CACERT_DIR", "priv/cert/cacert.pem")

# folder paths
config :ipncore, :data_dir, data_dir
config :ipncore, :kem_dir, kem_dir
config :ipncore, :falcon_dir, falcon_dir

# p2p server
config :ipncore, :p2p,
  handler_module: Ippan.P2P.Server,
  transport_module: ThousandIsland.Transports.TCP,
  port: 5815,
  num_acceptors: 10

config :ipncore, :node,
  id: id,
  name: hostname,
  port: 5815

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
