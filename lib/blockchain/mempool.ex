defmodule Mempool do
  @table :mempool
  @total_threads Application.get_env(:ipncore, :total_threads, System.schedulers_online())

  def open do
    :ets.whereis(@table)
    |> case do
      :undefined ->
        :ets.new(@table, [
          :ordered_set,
          :public,
          :named_table,
          read_concurrency: true,
          write_concurrency: true
        ])

      _ ->
        :ok
    end
  end

  defmacrop assign_worker_thread(from) do
    quote do
      rem(:binary.last(unquote(from)), @total_threads)
    end
  end

  # defmacrop assign_worker_thread(type, from, [first_body | _rest_body]) do
  #   quote do
  #     type_name = Ipncore.Event.type_name(unquote(type))

  #     target =
  #       case type_name do
  #         "tx." <> _ ->
  #           unquote(from)

  #         _ ->
  #           unquote(first_body)
  #       end

  #     :binary.last(target)
  #     |> rem(@total_threads)
  #   end
  # end

  # defmacrop assign_worker_thread(_type, from, _) do
  #   quote do
  #     unquote(from)
  #     |> :binary.last()
  #     |> rem(@total_threads)
  #   end
  # end

  def size, do: :ets.info(@table, :size)

  def push!(hash, time, type_number, from, body, signature, size) do
    thread = assign_worker_thread(from)

    # :ets.fun2ms(fn {{hash, time}, _thread, type, from, _body, _sigs, _size} = x when hash == 0 or type == 1 and from == 2 -> x end)
    case :ets.select(@table, [
           {{{:"$1", :"$2"}, :"$3", :"$4", :"$5", :"$6", :"$7", :"$8"},
            [
              {:orelse, {:==, :"$1", hash},
               {:andalso, {:==, :"$4", type_number}, {:==, :"$5", from}}}
            ], [:"$_"]}
         ]) do
      [] ->
        case :ets.insert_new(
               @table,
               {{time, hash}, thread, type_number, from, body, signature, size}
             ) do
          true ->
            {:ok, hash}

          false ->
            throw("Event already issued")
        end

      _ ->
        # means that this type of event has already been issued by the same issuer
        throw("Event already issued")
    end
  end

  def get(time, hash) do
    case :ets.lookup(@table, {time, hash}) do
      [obj] -> obj
      _ -> nil
    end
  end

  def last do
    case :ets.last(@table) do
      :"$end_of_table" ->
        nil

      key ->
        case :ets.lookup(@table, key) do
          [val] ->
            val

          _ ->
            nil
        end
    end
  end

  def select(thread, timestamp) do
    # :ets.fun2ms(fn {{time, _hash}, thread, _type, _from, _body, _sigs, _size} = x when thread == 0 and time <= 1 -> x end)
    fun = [
      {{{:"$1", :"$2"}, :"$3", :"$4", :"$5", :"$6", :"$7", :"$8"},
       [{:andalso, {:==, :"$3", thread}, {:"=<", :"$1", timestamp}}], [:"$_"]}
    ]

    :ets.select(@table, fun)
  end

  def select_delete(timestamp) do
    # :ets.fun2ms(fn {{time, _hash}, thread, _type, _from, _body, _sigs, _size} = x when and time <= 0 -> true end)
    fun = [
      {{{:"$1", :"$2"}, :"$3", :"$4", :"$5", :"$6", :"$7", :"$8"}, [{:"=<", :"$1", timestamp}],
       [true]}
    ]

    :ets.select_delete(@table, fun)
  end

  def delete(key) do
    :ets.delete(@table, key)
  end
end
