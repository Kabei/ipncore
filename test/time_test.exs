# ExUnit.start()
alias Ipncore.Chain
# import Ipncore.Chain, only: [get_tim: 0]
# Chain.put_iit(:erlang.system_time(:millisecond))

Benchee.run(
  %{
    "erl timestamp" => fn -> :erlang.system_time(:millisecond) end,
    "erl timestamp 1000" => fn -> :erlang.system_time(1000) end,
    "iit" => fn -> Chain.get_time() end,
    "monotonic" => fn -> :erlang.monotonic_time(:millisecond) end,
    "os timestamp" => fn -> :os.timestamp() end
  },
  time: 5,
  memory_time: 0,
  parallel: 4,
  print: [fast_warning: false]
)
