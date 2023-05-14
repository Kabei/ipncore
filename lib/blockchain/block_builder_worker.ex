defmodule BlockBuilderWork do
  require Logger
  alias Ipncore.{Block, Chain, Event}

  @unit_time Default.unit_time()
  @interval Default.block_interval()
  @timeout :infinity
  @total_threads Application.get_env(:ipncore, :total_threads, System.schedulers_online())

  def run do
    total_events = Mempool.size()

    if total_events != 0 do
      Logger.info("start block builder")
      start_time = :os.system_time(:microsecond)

      timestamp = :os.system_time(@unit_time)
      next_height = Chain.next_index()

      # ["BlockBuilder | Events: ", to_string(total_events)]
      # |> IO.iodata_to_binary()
      # |> Logger.debug()

      events =
        Enum.map(0..(@total_threads - 1), fn thread ->
          Task.async(fn ->
            events = Mempool.select(thread, timestamp)

            Enum.reduce(events, [], fn {{time, hash} = key, _thread, type_number, from, body,
                                        signature, size},
                                       acc ->
              try do
                ev = Event.new!(next_height, hash, time, type_number, from, body, signature, size)
                Mempool.delete(key)
                acc ++ [ev]
              rescue
                ex ->
                  Logger.error(Exception.format(:error, ex, __STACKTRACE__))
                  Mempool.delete(key)
                  acc
              catch
                err ->
                  Logger.error(err)
                  Mempool.delete(key)
                  acc
              end
            end)
          end)
        end)
        |> Task.await_many(@timeout)
        |> Enum.concat()
        |> Enum.sort(&({&1.time, &1.hash} <= {&2.time, &2.hash}))

      end_time = :os.system_time(:microsecond)
      Logger.info("end block builder: #{end_time - start_time} µs")
      Logger.info("Total events: #{length(events)}")

      # Mempool.select_delete_timestamp(timestamp)

      case events do
        [] ->
          Logger.info("Events empty")

        events ->
          last_block = Chain.last_block()
          block = Block.next(last_block, events)
          # Chain.add_block(last_block, block, Default.channel())
      end
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
