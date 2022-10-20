defmodule Default do
  # units
  def version, do: 0
  def unit_time, do: :millisecond
  def server_name, do: "IPPAN"
  def channel, do: Application.get_env(:ipncore, :channel)
  def token, do: "IPN"
  def token_name, do: "Instant Personal Network"
  def token_symbol, do: "Þ"
  def token_decimals, do: 9
  def interval, do: Application.get_env(:ipncore, :block_interval)

  # def address, do: Application.get_env(:ipncore, :address)

  # # types range
  # def wallet_type_range, do: 0..4
  # def content_type_range, do: 1..3
  # def post_privacy_type_range, do: 0..3

  # # length
  # def length_address, do: 25
  # def length_combined_address, do: 48
  # def length_email, do: 100
  # def length_username, do: 30
  # def length_biography, do: 150
  # def length_post_descri, do: 65_536

  # # mime
  # def mime_photo_allow, do: ~w(image/bmp image/jpeg image/png image/gif image/webp)

  # # auth
  # # def secret_access_token, do: "4dY45dpefdjfie58dSOR5HPdcywndioS"
  # # def secret_refresh_token, do: "pMi7NiMZN8GD7rqem20Sw39qLKfosydO"
  # def access_token_age, do: 3600
  # def refresh_token_age, do: 172_800

  # # tablespace
  # def ts_default, do: "pg_default"
  # def ts_blockchain, do: "pg_default"
  # def ts_chat, do: "pg_default"
  # def ts_social, do: "pg_default"
  # # def ts_blockchain, do: "ts_blockchain"
  # # def ts_chat, do: "ts_chat"
  # # def ts_social, do: "ts_social"
end
