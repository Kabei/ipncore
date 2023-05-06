Benchee.run(
  %{
    "erl system_time" => fn -> :erlang.system_time(:millisecond) end,
    "monotonic" => fn -> :erlang.monotonic_time(:millisecond) end,
    "os timestamp" => fn -> :os.timestamp() end,
    "os system_time" => fn -> :os.system_time(:millisecond) end
  },
  time: 5,
  memory_time: 0,
  print: [fast_warning: false]
)
