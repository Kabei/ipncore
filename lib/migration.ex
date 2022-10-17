defmodule Ipncore.Migration do
  alias Ipncore.{Channel, Repo}
  import Exto.Query

  alias Ipncore.Migration.{
    Blockchain,
    System
  }

  @otp_app :ipncore

  def start do
    migration_version = Application.get_env(@otp_app, :migration_version)
    # channel_version = Application.get_env(@otp_app, :channel_version)

    if Repo.schema_exists?("sys") do
      System.build(%{"version" => migration_version})
    else
      current_version =
        from(env in "env", where: env.name == "version", select: env.value, limit: 1)
        |> Repo.one(prefix: "sys") || 0

      if migration_version > current_version do
        System.build(%{"version" => migration_version})
      end

      channels = Channel.all()

      for channel <- channels do
        if Repo.schema_exists?(channel.id) do
          if channel_version > channel.vsn do
            Blockchain.build(%{"channel" => channel.id, "version" => channel_version})
          else
            throw("Migration versions are differents")
          end
        else
          Blockchain.build(%{"channel" => channel.id, "version" => channel_version})
        end
      end
    end
  end
end
