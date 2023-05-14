defmodule Ipncore.IIT do
  require Logger

  def sync do
    Logger.info("Sync time")

    if GpsReceiver.exists?() do
      gps_result = GpsReceiver.get_time()

      case gps_result do
        {:error, _} ->
          :os.system_time(:millisecond)

        datetime ->
          Logger.info("Time gps received: #{datetime}")
          DateTime.to_unix(datetime)
      end
    else
      Application.get_env(:ipncore, :ntp_servers)
      |> Enum.reduce_while(nil, fn domain, _acc ->
        SNTP.time(host: domain, timeout: 1000)
        |> case do
          {:ok, data} ->
            Logger.info("Time received from #{domain}")
            {:halt, trunc(data.originate_timestamp)}

          _err ->
            {:cont, nil}
        end
      end) || :os.system_time(:millisecond)
    end
  end
end
