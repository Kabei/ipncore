defmodule Ippan.P2P.Client do
  alias Phoenix.PubSub
  alias Ippan.Address
  use GenServer
  require Logger

  @module __MODULE__
  @adapter :gen_tcp
  @version <<0, 0>>
  @seconds <<0>>
  @tag_bytes 16
  @time_to_reconnect 3_000
  @handshake_timeout 5_000
  @ping_interval 30_000
  @pubsub_server :pubsub

  # {:ok, pid} = Ippan.P2P.Client.start_link('localhost', 5815, "priv/cert/falcon.key")
  # Ippan.P2P.Client.send(pid, "block.new", %{hash: "123456"})
  def start_link({hostname, port, key_path}) do
    seed =
      if File.regular?(key_path) do
        File.read!(key_path)
        |> Base.decode64!()
      else
        key_path
      end

    {:ok, pubkey, privkey} = Falcon.gen_keys_from_seed(seed)

    {:ok, pid} = GenServer.start_link(@module, {hostname, port, pubkey, privkey})

    {:ok, pid}
  end

  @impl true
  def init({hostname, port, falconPubkey, falconPrivkey}) do
    Process.flag(:trap_exit, true)

    address = Address.hash(1, falconPubkey)

    {:ok,
     %{
       hostname: hostname,
       port: port,
       address: address,
       pubkey: falconPubkey,
       privkey: falconPrivkey
     }, {:continue, :reconnect}}
  end

  @spec connect(pid()) :: {:ok, port()} | :error
  def connect(pid) do
    GenServer.call(pid, :connect, 15_000)
  end

  @spec reconnect(pid()) :: {:ok, port()}
  def reconnect(pid) do
    try do
      case connect(pid) do
        {:ok, socket} ->
          {:ok, socket}

        _ ->
          :timer.sleep(@time_to_reconnect)
          reconnect(pid)
      end
    catch
      :exit, _ ->
        :timer.sleep(@time_to_reconnect)
        reconnect(pid)
    end
  end

  @spec send(pid(), String.t(), map()) :: :ok
  def send(pid, event, msg) do
    message = Map.put(msg, :event, event)
    GenServer.cast(pid, {:send, message})
  end

  def stop(pid) do
    GenServer.call(pid, :stop)
  end

  @impl true
  def handle_continue(:reconnect, initial_state) do
    reconnect(self())
    {:noreply, initial_state}
  end

  @impl true
  def handle_info({:tcp, socket, "blockfile:ok:" <> block_id}, state) do
    block = BlockStore.lookup(block_id)
    receive_blockfile(socket, block)
    {:noreply, state}
  end

  def handle_info({:tcp, _socket, "blockfile:error"}, state) do
    {:noreply, state}
  end

  def handle_info({:tcp, _socket, data}, state) do
    message = decode(data, state)

    case message do
      %{event: event} when is_binary(event) and byte_size(event) <= 256 ->
        rest = Map.delete(message, :event)
        PubSub.broadcast(@pubsub_server, event, rest)

      _ ->
        Logger.debug("Not found")
    end

    {:noreply, state}
  end

  def handle_info({:tcp_closed, _socket}, %{hostname: hostname, port: port, tRef: tRef} = state) do
    Logger.debug("tcp_closed | #{hostname}:#{port}")
    :timer.cancel(tRef)
    reconnect(self())
    {:noreply, state}
  end

  def handle_info({:tcp_error, _socket, reason}, %{hostname: hostname, port: port} = state) do
    Logger.debug("tcp_error #{reason} | #{hostname}:#{port}")
    {:noreply, state}
  end

  def handle_info(:ping, %{socket: socket} = state) do
    @adapter.send(socket, "PING")
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:noreply, Map.put(state, :tRef, tRef)}
  end

  def handle_info({:EXIT, _pid, {:exit_trap, reason}}, state) do
    {:stop, reason, state}
  end

  @impl true
  def terminate(_reason, %{scoket: socket}) do
    @adapter.close(socket)
    :ok
  end

  @impl true
  def handle_call(:connect, _from, %{hostname: hostname, port: port} = state) do
    try do
      Logger.debug("#{hostname}:#{port} | connecting...")
      {:ok, socket} = @adapter.connect(hostname, port, [:binary, active: false])
      {:ok, sharedkey} = handshake(socket, state)
      {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
      :inet.setopts(socket, active: true)

      new_state = Map.merge(state, %{socket: socket, sharedkey: sharedkey, tRef: tRef})
      Logger.debug("#{hostname}:#{port} | connected")
      {:reply, {:ok, socket}, new_state}
    rescue
      MatchError ->
        {:reply, :error, state}
    end
  end

  def handle_call(:get_socket, _from, %{socket: socket} = state) do
    {:reply, socket, state}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, state}
  end

  @impl true
  def handle_cast({:send, data}, %{socket: socket} = state) do
    message = encode(data, state)
    @adapter.send(socket, message)

    {:noreply, state}
  end

  defp handshake(socket, state) do
    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL" <> @version <> pubkey} when byte_size(pubkey) == 1138 ->
        {:ok, ciphertext, sharedkey} = NtruKem.enc(pubkey)

        id = Default.validator_id()
        signature = Falcon.sign(state.privkey, sharedkey)
        authtext = encode(state.pubkey <> <<id::bytes-size(8)>> <> signature, sharedkey)
        @adapter.send(socket, "THX" <> ciphertext <> authtext)
        {:ok, sharedkey}

      error ->
        error
    end
  end

  defp encode(msg, sharedkey) do
    bin = :erlang.term_to_binary(msg)
    iv = :crypto.strong_rand_bytes(12)

    {ciphertext, tag} =
      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        sharedkey,
        iv,
        bin,
        @seconds,
        @tag_bytes,
        true
      )

    iv <> tag <> ciphertext
  end

  defp decode(packet, sharedkey) do
    try do
      <<iv::bytes-size(12), tag::bytes-size(16), ciphertext::binary>> = packet

      :crypto.crypto_one_time_aead(
        :chacha20_poly1305,
        sharedkey,
        iv,
        ciphertext,
        @seconds,
        tag,
        false
      )
      |> :erlang.binary_to_term([:safe])
    rescue
      _error ->
        :error
    end
  end

  def receive_blockfile(socket, block_info) do
    size = block_info.size
    filename = :filename.absname_join("data/blocks", block_info.id)
    {:ok, file_handle} = File.open(filename, [:write, :binary])
    hash_state = :crypto.hash_init(:sha)

    case receive_blockfile(socket, hash_state, file_handle, size) do
      {:ok, filehash} ->
        filehash == block_info.filehash

      _ ->
        :file.delete(filename)
        false
    end
  end

  defp receive_blockfile(_, hash_state, file_handle, 0) do
    # IO.puts("Finished Receiving File")
    :file.close(file_handle)
    {:ok, :crypto.hash_final(hash_state)}
  end

  defp receive_blockfile(socket, hash_state, file_handle, size) do
    {n_size, to_read} =
      if size - 1024 >= 0 do
        {size - 1024, 1024}
      else
        {0, 1024 - size}
      end

    case :gen_tcp.recv(socket, to_read) do
      {:ok, data} ->
        case :file.write(file_handle, data) do
          :ok ->
            hash_state = :crypto.hash_update(hash_state, data)
            receive_blockfile(socket, hash_state, file_handle, n_size)

          {:error, error} ->
            Logger.debug("Error writting file: #{inspect(error)}")
            :file.close(file_handle)
        end

      {:error, reason} ->
        IO.puts("Received finished with:")
        Logger.debug("Received finished with: #{inspect(reason)}")
        IO.puts(reason)
        :file.close(file_handle)
    end
  end
end
