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
config :ipncore, :token, System.get_env("NATIVE_TOKEN", "IPN")
config :ipncore, :message_max_size, 8192
config :ipncore, :version, 0
config :ipncore, :max_block_size, 10_485_760
config :ipncore, :max_block_data_size, 10_000_000
config :ipncore, :block_interval, :timer.seconds(5)
config :ipncore, :round_timeout, :timer.seconds(10)
config :ipncore, :block_extension, "block"
config :ipncore, :note_max_size, 255
config :ipncore, :max_tx_amount, 1_000_000_000_000_000
config :ipncore, :timeout_refund, :timer.hours(72)
config :ipncore, :message_timeout, :timer.seconds(5)
config :ipncore, :max_validators, 1000
config :ipncore, :max_tokens, 10_000
config :ipncore, :max_peers_conn, 20

# P2P client
config :ipncore, :p2p_client, [
  :binary,
  active: false,
  reuseaddr: true,
  packet: 2,
  packet_size: 64_000
]

config :ipncore, json: Jason
config :blake3, rayon: true
