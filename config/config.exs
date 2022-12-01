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

# blockchain 
config :ipncore, :migration_version, 0

config :ipncore, :event_version, 0
config :ipncore, :event_threshold_timeout, :timer.seconds(10)
config :ipncore, :event_max_size, 8192

config :ipncore, :block_version, 0
config :ipncore, :block_interval, :timer.seconds(5)

config :ipncore, :timeout_refund, :timer.hours(72)
config :ipncore, :epoch, 120_960
# config :ipncore, :tx_edit_delay, :timer.hours(1)
