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
  @pubsub_server :network

  # {:ok, pid} = Ippan.P2P.Client.start_link('localhost', 5815, "priv/secret.key")
  # Ippan.P2P.Client.send(pid, "block.new", %{hash: "123456"})
  def start_link({hostname, port, key_path}) do
    seed =
      if File.regular?(key_path) do
        File.read!(key_path)
        |> Fast64.decode64()
      else
        key_path
      end

    {:ok, {pubkey, privkey}} = Cafezinho.Impl.keypair_from_seed(seed)

    {:ok, pid} =
      GenServer.start_link(@module, {hostname, port, pubkey, privkey}, hibernate_after: 10_000)

    {:ok, pid}
  end

  @impl true
  def init({hostname, port, pubkey, privkey}) do
    Process.flag(:trap_exit, true)

    address = Address.hash(0, pubkey)

    {:ok,
     %{
       pid: self(),
       hostname: hostname,
       port: port,
       address: address,
       pubkey: pubkey,
       privkey: privkey
     }, {:continue, :reconnect}}
  end

  # @spec connect(pid()) :: {:ok, port()} | :error
  # def connect(pid) do
  #   IO.inspect("reconnect")
  #   GenServer.call(pid, :connect, :infinity)
  # end

  # @spec reconnect(pid()) :: {:ok, port()}
  # def reconnect(pid) do
  #   try do
  #     case connect(pid) do
  #       {:ok, socket} ->
  #         {:ok, socket}

  #       error ->
  #         IO.inspect("reconnect error")
  #         IO.inspect(error)
  #         :timer.sleep(@time_to_reconnect)
  #         reconnect(pid)
  #     end
  #   catch
  #     :exit, m ->
  #       IO.inspect(m)
  #       :timer.sleep(@time_to_reconnect)
  #       reconnect(pid)
  #   end
  # end

  @spec push(pid(), String.t(), term()) :: :ok
  def push(pid, event, msg) do
    GenServer.cast(pid, {:push, {event, msg}})
  end

  def stop(pid) do
    GenServer.stop(pid, :normal)
  end

  @impl true
  def handle_continue(:reconnect, initial_state) do
    IO.inspect("continue")
    :timer.send_after(@time_to_reconnect, :reconnect)
    {:noreply, initial_state}
  end

  @impl true
  def handle_info(:reconnect, state) do
    IO.inspect("reconnect")

    try do
      case connect(state) do
        {:ok, new_state} ->
          {:noreply, new_state}

        error ->
          IO.inspect("reconnect error")
          IO.inspect(error)
          :timer.send_after(@time_to_reconnect, :reconnect)
          {:noreply, state}
      end
    catch
      :exit, m ->
        IO.inspect(m)
        :timer.send_after(@time_to_reconnect, :reconnect)
        {:noreply, state}
    end
  end

  def handle_info({:tcp, _socket, data}, state) do
    message = decode(data, state)

    case message do
      {event, data} ->
        PubSub.broadcast(@pubsub_server, event, data)

      _ ->
        Logger.debug("Not found")
    end

    {:noreply, state}
  end

  def handle_info(
        {:tcp_closed, _socket},
        %{hostname: hostname, port: port, tRef: tRef} = state
      ) do
    Logger.debug("tcp_closed | #{hostname}:#{port}")
    :timer.cancel(tRef)
    :timer.send_after(@time_to_reconnect, :reconnect)
    {:noreply, state}
  end

  def handle_info(
        {:tcp_error, _socket, reason},
        %{hostname: hostname, port: port} = state
      ) do
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

  # def handle_info({event, action, message}, state) do

  #   {:noreply, state}
  # end

  @impl true
  def terminate(_reason, %{scoket: socket}) do
    @adapter.close(socket)
  end

  def terminate(_reason, _) do
    :ok
  end

  @impl true
  def handle_call(:get_socket, _from, %{socket: socket} = state) do
    {:reply, socket, state}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, state}
  end

  @impl true
  def handle_cast({:push, data}, %{socket: socket} = state) do
    message = encode(data, state)
    @adapter.send(socket, message)

    {:noreply, state}
  end

  defp connect(%{hostname: hostname, port: port} = state) do
    IO.inspect("connecting")

    try do
      Logger.info("#{hostname}:#{port} | connecting...")

      {:ok, ip_addr} = :inet_udp.getaddr(String.to_charlist(hostname))

      {:ok, socket} = @adapter.connect(ip_addr, port, [:binary])
      :inet.setopts(socket, active: false)
      {:ok, sharedkey} = handshake(socket, state)
      {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
      :ok = :inet.setopts(socket, active: true)

      new_state = Map.merge(state, %{socket: socket, sharedkey: sharedkey, tRef: tRef})
      Logger.debug("#{hostname}:#{port} | connected")
      # {:reply, {:ok, socket}, new_state}
      {:ok, new_state}
    rescue
      MatchError ->
        # {:reply, :error, state}
        {:error, state}
    end
  end

  defp handshake(socket, state) do
    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL" <> @version <> pubkey} ->
        IO.inspect("Welcome #{byte_size(pubkey)}")
        {:ok, ciphertext, sharedkey} = NtruKem.enc(pubkey)

        id = Default.validator_id()
        {:ok, signature} = Cafezinho.Impl.sign(sharedkey, state.privkey)
        authtext = encode(state.pubkey <> <<id::64>> <> signature, sharedkey)
        @adapter.send(socket, "THX" <> ciphertext <> authtext)
        {:ok, sharedkey}

      error ->
        IO.inspect("error")
        IO.inspect(error)
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
end