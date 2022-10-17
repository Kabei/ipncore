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

# Ipnutils lib
config :ipnutils, :jsonlib, Jason
config :ipnutils, :repo, Ipncore.Repo

# blcokchain config
config :ipncore, :migration_version, 0
config :ipncore, :block_version, 0
config :ipncore, :block_interval, :timer.seconds(60)
config :ipncore, :tx_version, 0
config :ipncore, :tx_timeout, :timer.seconds(30)
config :ipncore, :tx_timeout_refund, :timer.hours(72)
config :ipncore, :tx_edit_delay, :timer.hours(1)
