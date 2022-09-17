defmodule Ipncore.IIT do
  require Logger

  @ntp_servers [
    '0.north-america.pool.ntp.org',
    '1.north-america.pool.ntp.org',
    '2.north-america.pool.ntp.org',
    '0.europe.pool.ntp.org',
    '1.europe.pool.ntp.org',
    '2.europe.pool.ntp.org',
    '0.asia.pool.ntp.org',
    '1.asia.pool.ntp.org',
    '2.asia.pool.ntp.org',
    '0.oceania.pool.ntp.org',
    '0.africa.pool.ntp.org',
    'hora.roa.es',
    'time.google.com',
    'time.cloudflare.com',
    'time.windows.com'
  ]

  def sync do
    Logger.info("Sync time")

    if GpsReceiver.exists?() do
      gps_result = GpsReceiver.get_time()

      case gps_result do
        {:error, _} ->
          :erlang.system_time(:millisecond)

        datetime ->
          Logger.info("Time gps received: #{datetime}")
          DateTime.to_unix(datetime)
      end
    else
      Enum.reduce_while(@ntp_servers, nil, fn domain, _acc ->
        SNTP.time(host: domain, timeout: 1000)
        |> case do
          {:ok, data} ->
            Logger.info("Time received from #{domain}")
            {:halt, trunc(data.originate_timestamp)}

          _err ->
            {:cont, nil}
        end
      end) || :erlang.system_time(:millisecond)
    end
  end
end
