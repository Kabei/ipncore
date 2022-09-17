Benchee.run(
  %{
    "int to binary" => fn -> :binary.encode_unsigned(54_000) end,
    "int to string" => fn -> to_string(54_000) end,
    "int to string2" => fn -> Integer.to_string(54_000) end,
  },
  # time: 5,
  # memory_time: 0,
  parallel: 8,
  print: [fast_warning: false]
)
