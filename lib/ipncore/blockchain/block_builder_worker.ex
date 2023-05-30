defmodule BlockBuilderWork do
  # alias Redix.PubSub
  # require Logger

  # @unit_time Default.unit_time()
  # @interval Default.block_interval()
  @interval 60_000
  # @timeout :infinity
  # @pubsub_server :pubsub

  def sync_all do
    MessageStore.sync()
    WalletStore.sync()
    ValidatorStore.sync()
    BalanceStore.sync()
    DomainStore.sync()
    DnsStore.sync()
    TokenStore.sync()
    EnvStore.sync()
    RefundStore.sync()
    BlockStore.sync()
    RoundStore.sync()
    EnvStore.sync()
  end

  def run do
    spawn(fn ->
      sync_all()
      :timer.send_after(@interval, "next")

      receive do
        "next" ->
          run()
      end
    end)
  end

  # def run do
  # last_block = Chain.get_last_block()
  # {:ok, records} = MessageStore.execute_fetch("by_block")

  # current_block_number = last_block.height + 1
  # data_dir = Application.get_env(@otp_app, :data_dir, "data")

  # bytes = :erlang.term_to_binary(records)

  # path = Path.join(data_dir, "/blocks/#{last_block}")
  # {:ok, file} = :file.open(path)
  # :file.write(file, bytes)

  # PubSub.broadcast(@pubsub_server, "block.new", block)
  # end

  # def next do
  #   spawn(fn ->
  #     :timer.sleep(@interval)
  #     BlockBuilderWork.run()
  #   end)
  # end
end

# defmodule BlockBuilderWork do
#   require Logger
#   alias Ipncore.{Block, Chain, Event}

#   @unit_time Default.unit_time()
#   @interval Default.block_interval()
#   @timeout :infinity

#   def run do
#     total_events = Mempool.size()

#     if total_events != 0 do
#       total_threads = Application.get_env(:ipncore, :total_threads, System.schedulers_online())
#       timestamp = :erlang.system_time(@unit_time)
#       next_height = Chain.next_index()

#       ["BlockBuilder | Events: ", to_string(total_events)]
#       |> IO.iodata_to_binary()
#       |> Logger.debug()

#       events =
#         Enum.map(0..(total_threads - 1), fn thread ->
#           Task.async(fn ->
#             events = Mempool.select(thread, timestamp)

#             Enum.reduce(events, [], fn {{time, hash} = key, _thread, type_number, from, body,
#                                         signature, size},
#                                        acc ->
#               try do
#                 ev = Event.new!(next_height, hash, time, type_number, from, body, signature, size)
#                 acc ++ [ev]
#               rescue
#                 ex ->
#                   Logger.error(Exception.format(:error, ex, __STACKTRACE__))
#                   Mempool.delete(key)
#                   acc
#               catch
#                 err ->
#                   Logger.error(err)
#                   Mempool.delete(key)
#                   acc
#               end
#             end)
#           end)
#         end)
#         |> Task.await_many(@timeout)
#         |> Enum.concat()
#         |> Enum.sort(&({&1.time, &1.hash} <= {&2.time, &2.hash}))

#       IO.inspect("Events: #{length(events)}")

#       Mempool.select_delete_timestamp(timestamp)

#       case events do
#         [] ->
#           Logger.info("Events empty")

#         events ->
#           last_block = Chain.last_block()
#           block = Block.next(last_block, events)
#           Chain.add_block(last_block, block, Default.channel())
#       end
#     end

#     next()
#   end

#   def next do
#     spawn(fn ->
#       :timer.sleep(@interval)
#       BlockBuilderWork.run()
#     end)
#   end
# end
