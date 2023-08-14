defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  @input = [
    301,
    1_684_825_345_008,
    "0x3jRrjwFU6EqYSdHYS9byJhhDygob",
    "0xUoYd2fz9DjrKnWkDiWkRyCqh2gn",
    "IPN",
    [12, [45, 45564546], "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=", 454568.456458],
    %{height: 0, hash: "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=", hashfile: "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=", signature: "teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=teOoJc+BB69mW+WCW/VNfktPY0LEG//yiOyFYfdDMMk=", ev_count: 0},
    50000
  ]

  test "decode" do
    # data = "[301,1684825345008,[\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]]"
    # data = "[301,1684825345008,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]"
    # data = "[400,1685077454239,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",[\"example.ipn\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\",2,{\"avatar\":\"https://avatar.com\",\"email\":\"asd@example.com\"}]]"

    input = @input

    json = Jason.encode!(input)
    msgx = :msgpack.pack(input)
    cbor = CBOR.encode(input)
    ecbor = :ecbor.encode(input)

    Benchee.run(%{
      "jason" => fn -> Jason.decode!(json) end,
      "msgpack" => fn -> :msgpack.unpack(msgx, use_nil: true) end,
      "cbor" => fn -> CBOR.decode(cbor) end,
      "ecbor" => fn -> :ecbor.decode(ecbor) end
    })
  end

  test "encode" do
    # data = "[301,1684825345008,[\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]]"
    # data = "[301,1684825345008,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]"
    # data = "[400,1685077454239,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",[\"example.ipn\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\",2,{\"avatar\":\"https://avatar.com\",\"email\":\"asd@example.com\"}]]"

    input = @input

    Benchee.run(%{
      "jason" => fn -> Jason.encode!(input) end,
      "msgpack" => fn -> :msgpack.pack(input, use_nil: true) end,
      "cbor" => fn -> CBOR.encode(input) end,
      "ecbor" => fn -> :ecbor.encode(input) end
    })
  end
end
