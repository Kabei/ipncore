defmodule Default do
  @block_interval Application.compile_env(:ipncore, :block_interval)

  # units
  def version, do: 0
  def unit_time, do: :millisecond
  def server_name, do: "IPPAN"
  def channel, do: Application.get_env(:ipncore, :channel)
  def token, do: "IPN"
  def token_name, do: "Instant Personal Network"
  def token_symbol, do: "Þ"
  def token_decimals, do: 9
  def block_interval, do: @block_interval
  def imposible_address, do: <<0::160>>
  def address, do: Application.get_env(:ipncore, :address)
  def address58, do: Application.get_env(:ipncore, :address58)
  def data_dir, do: Application.get_env(:ipncore, :data_path, "data")
end
