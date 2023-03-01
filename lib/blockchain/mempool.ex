defmodule Mempool do
  use ETSTable,
    name: :mempool,
    ets_opts: [
      :ordered_set,
      :public,
      :named_table,
      read_concurrency: true,
      write_concurrency: true
    ]

  alias Ipncore.{Address, Event}

  @total_threads Application.get_env(:ipncore, :total_threads, System.schedulers_online())

  defmacrop assign_worker_thread(from) do
    quote do
      rem(:binary.last(unquote(from)), @total_threads)
    end
  end

  def push!(time, hash, type_number, from, body, signature, size) do
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

  def lookup(time, hash) do
    lookup({time, hash})
  end

  def select(thread, timestamp) do
    # :ets.fun2ms(fn {{time, _hash}, thread, _type, _from, _body, _sigs, _size} = x when thread == 0 and time <= 1 -> x end)
    fun = [
      {{{:"$1", :"$2"}, :"$3", :"$4", :"$5", :"$6", :"$7", :"$8"},
       [{:andalso, {:==, :"$3", thread}, {:"=<", :"$1", timestamp}}], [:"$_"]}
    ]

    select(fun)
  end

  def select_delete_timestamp(timestamp) do
    # :ets.fun2ms(fn {{time, _hash}, _thread, _type, _from, _body, _sigs, _size} when time <= 0 -> true end)
    fun = [
      {{{:"$1", :"$2"}, :"$3", :"$4", :"$5", :"$6", :"$7", :"$8"}, [{:"=<", :"$1", timestamp}],
       [true]}
    ]

    select_delete(fun)
  end

  def all do
    :ets.foldl(fn elem, acc -> acc ++ to_map(elem) end, [], @table)
  end

  def to_map({{time, hash}, type_number, from, body, signature, size}) do
    %{
      body: body,
      from: Address.to_text(from),
      hash: Base.decode16!(hash, case: :lower),
      size: size,
      signature: Base.decode64!(signature),
      type: Event.type_name(type_number),
      timestamp: time
    }
  end
end
