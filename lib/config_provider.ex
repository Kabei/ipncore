defmodule Ipncore.ConfigProvider do
  @behaviour Config.Provider
  @otp_app :ipncore

  # Let's pass the path to the JSON file as config
  @impl true
  def init(path) when is_binary(path), do: path

  @impl true
  def load(config, path) do
    # We need to start any app we may depend on.
    {:ok, _} = Application.ensure_all_started(:jason)

    json = path |> Path.expand() |> File.read!() |> Jason.decode!() |> to_key_atom()

    Config.Reader.merge(
      config,
      [
        {:channel, json[:channel]},
        {:gps_device, json[:gps_device]},
        {:central, json[:central]},
        {:pool, json[:pool]},
        {:web, json[:web]},
        {Ipncore.Repo, json[:db]}
      ]
    )
  end

  def read do
    result =
      load(Application.get_all_env(@otp_app), System.get_env("CONFIG_PATH", "./config.json"))

    Application.put_all_env([{@otp_app, result}])
  end

  defp to_key_atom(x) when is_map(x) do
    for {k, v} <- x do
      {String.to_atom(k), to_key_atom(v)}
    end
  end

  defp to_key_atom(x), do: x
end
