defmodule Session do
  @table :ssid_cache
  @hash :sha256
  @seed System.get_env("SESSION_SEED", "seed")
  @unit_time :millisecond
  @expiry 86_400_000

  defmacro if_expired(time1, time2, do: expression, else: expression2) do
    quote do
      case abs(unquote(time2) - unquote(time1)) > @expiry do
        true ->
          unquote(expression)

        false ->
          unquote(expression2)
      end
    end
  end

  defmacro unix_time do
    quote do
      :erlang.system_time(@unit_time)
    end
  end

  def name, do: @table

  def init do
    :ets.new(@table, [
      :set,
      # :duplicate_bag,
      :public,
      :named_table,
      read_concurrency: true,
      write_concurrency: true
    ])
  end

  def genID(kem_public_key) do
    :crypto.hash(@hash, kem_public_key <> @seed)
    |> :binary.part(0, 20)
  end

  def put(id, shared_key, conn) do
    struct = %{ip: conn.remote_ip, sk: shared_key, time: unix_time()}

    :ets.insert(@table, {id, struct})
  end

  def put_time(id) do
    time = unix_time()
    {id, struct} = get(id)

    if_expired(time, struct.time) do
      delete(id)
      :error
    else
      :ets.insert(@table, {id, Map.put(struct, :time, time)})
      :ok
    end
  end

  def get(id) do
    case :ets.lookup(@table, id) do
      [] ->
        nil

      [{key, data} | _] ->
        {key, data}
    end
  end

  def getEx(id) do
    case :ets.lookup(@table, id) do
      [] ->
        nil

      [{key, data} | _] ->
        time = unix_time()

        if_expired(time, data.time) do
          delete(id)
          nil
        else
          {key, data}
        end
    end
  end

  def delete(id) do
    :ets.delete(@table, id)
  end
end
