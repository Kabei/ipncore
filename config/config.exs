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

config :ipncore, :block_version, 0
config :ipncore, :migration_version, 0
config :ipncore, :block_interval, 60_000
config :ipncore, :tx_version, 0
config :ipncore, :tx_timeout, 60_000
