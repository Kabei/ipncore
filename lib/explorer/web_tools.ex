defmodule Ipncore.WebTools do
  import Plug.Conn, only: [send_resp: 3, put_resp_content_type: 2]
  import Ipnutils.Errors, only: [msg: 1]

  def json(conn, data) do
    body = Jason.encode!(data)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, body)
  end

  def json(conn, code, data) do
    body = Jason.encode!(data)

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(code, body)
  end

  def send_error(conn, code) do
    send_resp(conn, 400, msg(code))
  end

  def send_result(conn, nil) do
    send_resp(conn, 204, "")
  end

  def send_result(conn, []) do
    send_resp(conn, 204, "")
  end

  def send_result(conn, {_, []}) do
    send_resp(conn, 204, "")
  end

  def send_result(conn, {:ok, data}) do
    json(conn, data)
  end

  def send_result(conn, {:error, data}) do
    json(conn, data)
  end

  def send_result(conn, data) do
    json(conn, data)
  end
end
