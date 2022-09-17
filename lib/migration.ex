defmodule Ipncore.Migration do
  alias Ipncore.{Channel, Repo}

  alias Ipncore.Migration.{
    Blockchain,
    System
  }

  def start(%{"version" => version} = params) do
    unless Repo.schema_exists?("sys") do
      System.build(params)
    else
      channels = Channel.all()

      for channel <- channels do
        unless Repo.schema_exists?(channel.id) do
          Blockchain.build(%{"channel" => channel.id, "version" => version})
        end
      end
    end
  end
end
