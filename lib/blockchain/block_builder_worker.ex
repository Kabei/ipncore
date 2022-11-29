defmodule BlockBuilderWork do
  require Logger
  alias Ipncore.{Block, Chain, Event, Mempool}

  @unit_time Default.unit_time()
  @interval Default.block_interval()
  @channel Default.channel()
  @timeout :infinity

  def run do
    total_events = Mempool.size()

    if total_events != 0 do
      total_threads = Application.get_env(:ipncore, :total_threads, System.schedulers_online())
      timestamp = :erlang.system_time(@unit_time)
      next_height = Chain.next_index()
      ["BlockBuilder | Events: ", total_events] |> IO.iodata_to_binary() |> Logger.debug()

      events =
        Enum.map(0..(total_threads - 1), fn thread ->
          Task.async(fn ->
            events = Mempool.select(thread, timestamp)

            Enum.reduce(events, [], fn {{time, hash} = key, _thread, type_number, from, body,
                                        signature, size},
                                       acc ->
              try do
                ev = Event.new!(next_height, hash, time, type_number, from, body, signature, size)
                acc ++ [ev]
              rescue
                _ex ->
                  Mempool.delete(key)
                  acc
              catch
                _err ->
                  Mempool.delete(key)
                  acc
              end
            end)

            :ok
          end)
        end)
        |> Task.await_many(@timeout)
        |> Enum.concat()
        |> Enum.sort(&({&1.time, &1.hash} <= {&2.time, &2.hash}))

      Mempool.select_delete(timestamp)

      last_block = Chain.last_block()
      block = Block.new(last_block, events)
      Chain.add_block(last_block, block, @channel)
    end

    next()
  end

  def next do
    spawn(fn ->
      :timer.sleep(@interval)
      BlockBuilderWork.run()
    end)
  end
end
