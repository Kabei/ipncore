defmodule Ipncore.DNS.TlsServer do
    use ThousandIsland.Handler
  
    @impl ThousandIsland.Handler
    def handle_connection(_socket, state) do
    #   IO.inspect("connection TLS")
      {:continue, state}
    end
  
    @impl ThousandIsland.Handler
    def handle_data(<<_first::bytes-size(2), rest::binary>>, socket, state) do
      # IO.inspect(data)
      result = Ipncore.DNS.handle(rest, 0)
      ThousandIsland.Socket.send(socket, <<byte_size(result)::16>> <> result)
      {:continue, state}
    end
  
    def handle_data(_data, _socket, state) do
      {:continue, state}
    end
  
    @impl ThousandIsland.Handler
    def handle_close(_socket, _state), do: :ok
    @impl ThousandIsland.Handler
    def handle_error(_error, _socket, _state), do: :ok
    @impl ThousandIsland.Handler
    def handle_shutdown(_socket, _state), do: :ok
    @impl ThousandIsland.Handler
    def handle_timeout(_socket, _state), do: :ok
  end