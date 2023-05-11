defmodule Global do
  use GlobalConst.DummyModule
end

defmodule Default do
  @block_interval Application.compile_env(:ipncore, :block_interval)
  @token Application.compile_env(:ipncore, :token)

  # units
  def version, do: 0
  def unit_time, do: :millisecond
  def token, do: @token
  def block_interval, do: @block_interval
  def imposible_address, do: <<0::160>>

  def node_name, do: Application.get_env(:ipncore, :node)[:name]
  def validator_id, do: Application.get_env(:ipncore, :node)[:id]
  def hostname, do: Application.get_env(:ipncore, :node)[:hostname]
end
