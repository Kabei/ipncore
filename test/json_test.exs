defmodule RequestTest do
  use ExUnit.Case
  doctest Ipncore

  test "json" do
    # data = "[301,1684825345008,[\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]]"
    data = "[301,1684825345008,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\", \"IPN\", 50000]"
    # data = "[400,1685077454239,\"0x3jRrjwFU6EqYSdHYS9byJhhDygob\",[\"example.ipn\",\"0xUoYd2fz9DjrKnWkDiWkRyCqh2gn\",2,{\"avatar\":\"https://avatar.com\",\"email\":\"asd@example.com\"}]]"

    Benchee.run(%{
      "jason" => fn -> Jason.decode!(data) end,
      "jsonrs" => fn -> Jsonrs.decode!(data) end
    })
  end
end
