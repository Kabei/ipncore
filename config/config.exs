import Config

prod = config_env() == :prod

if prod do
  config :logger,
    backends: [:console],
    level: :info,
    compile_time_purge_matching: [
      [level_lower_than: :info]
    ]
else
  config :logger, level: :debug
end

# Blockchain setup
block_interval = :timer.seconds(5)
config :ipncore, :token, System.get_env("NATIVE_TOKEN", "IPN")
config :ipncore, :message_max_size, 8192
config :ipncore, :block_version, 0
config :ipncore, :block_max_size, 10_485_760
config :ipncore, :block_data_max_size, 10_000_000
config :ipncore, :block_interval, block_interval
config :ipncore, :note_max_size, 255
config :ipncore, :note_max_size, 1_000_000_000_000_000
# config :ipncore, :last_activity, :timer.hours(24)
config :ipncore, :timeout_refund, :timer.hours(72)
config :ipncore, :message_timeout, :timer.seconds(15)
config :ipncore, :delete_expiry, div(86400, block_interval)

# P2P client
config :ipncore, :p2p_client, [
  :binary,
  reuseaddr: true,
  packet: 2,
  packet_size: 9_000
]

config :ipncore, json: Jason
config :blake3, rayon: true
