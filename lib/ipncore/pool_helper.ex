defmodule PoolHelper do
  @schema "http://"
  @info_fields ~w(address name channel central fee fee_percent)

  @spec info! :: Keyword.t()
  def info! do
    pool = Application.get_env(:ipncore, :pool)

    cond do
      not is_nil(pool) ->
        # catch pool info
        pool

      true ->
        imp_client = Application.get_env(:ipncore, :imp_client)
        hostname = imp_client[:host]
        url = @schema <> hostname <> "/info"
        HTTPoison.start()

        case HTTPoison.get(url) do
          {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
            kw_info =
              Jason.decode!(body)
              |> Map.take(@info_fields)
              # |> Map.put(:timestamp, :erlang.system_time(:millisecond))
              |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)

            # put pool info
            Application.put_env(:ipncore, :pool, kw_info)

            # # put address in imp_client
            Application.put_env(
              :ipncore,
              :imp_client,
              Keyword.put(imp_client, :address, kw_info[:address])
            )

            kw_info

          {:ok, %HTTPoison.Response{status_code: _}} ->
            throw("Error catch info pool")

          {:error, %HTTPoison.Error{reason: _reason}} ->
            throw("Fatal error catch info pool")
        end
    end
  end
end
