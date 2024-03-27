defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  @input = [
    301,
    1_684_825_345_008,
    "0x3jRrjwFU6EqYSdHYS9byJhhDygob",
    "0xUoYd2fz9DjrKnWkDiWkRyCqh2gn",
    "IPN",
    [12, [45, 45_564_546], "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=", 454_568.456458],
    %{
      height: 0,
      hash: "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=",
      filehash: "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=",
      signature:
        "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=",
      ev_count: 0
    },
    50000
  ]

  test "decode" do
    # data = "[301,1684825345008,[\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]]"
    # data = "[301,1684825345008,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]"
    # data = "[400,1685077454239,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",[\"example.ipn\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\",2,{\"image\":\"https://image.com\",\"email\":\"asd@example.com\"}]]"

    input = @input

    json = Jason.encode!(input)
    # msgx = :msgpack.pack(input)
    cbor = CBOR.encode(input)
    erl = :erlang.term_to_binary(input)

    Benchee.run(%{
      "jason" => fn -> Jason.decode!(json) end,
      # "msgpack" => fn -> :msgpack.unpack(msgx, use_nil: true) end,
      "cbor" => fn -> CBOR.decode(cbor) end,
      "erl" => fn -> :erlang.binary_to_term(erl) end
    })
  end

  test "encode" do
    # data = "[301,1684825345008,[\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]]"
    # data = "[301,1684825345008,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]"
    # data = "[400,1685077454239,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",[\"example.ipn\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\",2,{\"image\":\"https://image.com\",\"email\":\"asd@example.com\"}]]"

    input = @input

    Benchee.run(%{
      "jason" => fn -> Jason.encode!(input) end,
      # "msgpack" => fn -> :msgpack.pack(input, use_nil: true) end,
      "cbor" => fn -> CBOR.encode(input) end,
      "erl" => fn -> :erlang.term_to_binary(input) end
      # "ecbor" => fn -> :ecbor.encode(input) end
    })
  end
end
