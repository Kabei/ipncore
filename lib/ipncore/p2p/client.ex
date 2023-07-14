defmodule Ippan.P2P.Client do
  alias Phoenix.PubSub
  alias Ippan.Address
  use GenServer
  require Logger

  @module __MODULE__
  @adapter :gen_tcp
  @version <<0, 0>>
  @seconds <<0>>
  @iv_bytes 12
  @tag_bytes 16
  @time_to_reconnect 3_000
  @handshake_timeout 5_000
  @ping_interval 40_000
  @pubsub_server :network

  def start_link({hostname, port, vid, key_path}) do
    seed =
      if File.regular?(key_path) do
        File.read!(key_path)
        |> Fast64.decode64()
      else
        key_path
      end

    {:ok, {pubkey, privkey}} = Cafezinho.Impl.keypair_from_seed(seed)

    {:ok, pid} =
      GenServer.start_link(@module, {hostname, port, pubkey, vid, privkey},
        hibernate_after: 10_000
      )

    {:ok, pid}
  end

  @impl true
  def init({hostname, port, pubkey, vid, privkey}) do
    Process.flag(:trap_exit, true)

    address = Address.hash(0, pubkey)

    subscribe(vid)

    {:ok,
     %{
       id: vid,
       pid: self(),
       hostname: hostname,
       port: port,
       address: address,
       pubkey: pubkey,
       privkey: privkey,
       mailbox: %{}
     }, {:continue, :reconnect}}
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

  def handle_info({:tcp, _socket, data}, %{sharedkey: sharedkey} = state) do
    message = decode(data, sharedkey)

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
        %{hostname: hostname, socket: socket, port: port, tRef: tRef} = state
      ) do
    Logger.debug("tcp_closed | #{hostname}:#{port}")
    :timer.cancel(tRef)
    @adapter.close(socket)
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

  def handle_info({"clear", id}, %{mailbox: mailbox} = state) do
    {:noreply, %{state | mailbox: Map.delete(mailbox, id)}}
  end

  def handle_info({_event, _action, data} = msg, %{mailbox: mailbox} = state) do
    case state do
      %{socket: socket, sharedkey: sharedkey} ->
        @adapter.send(socket, encode(msg, sharedkey))

      _ ->
        :ok
    end

    if is_map(data) do
      Logger.debug(inspect(data))
      {:noreply, %{state | mailbox: Map.put(mailbox, data.height, msg)}}
    else
      {:noreply, %{state | mailbox: Map.put(mailbox, data, msg)}}
    end
  end

  def handle_info(_, state) do
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %{id: vid, scoket: socket}) do
    unsubscribe(vid)
    @adapter.close(socket)
  end

  def terminate(_reason, %{id: vid}) do
    unsubscribe(vid)
  end

  @impl true
  def handle_call(:get_socket, _from, %{socket: socket} = state) do
    {:reply, socket, state}
  end

  def handle_call(:stop, _from, state) do
    {:stop, :normal, state}
  end

  defp connect(%{hostname: hostname, port: port} = state) do
    try do
      Logger.info("#{hostname}:#{port} | connecting...")

      {:ok, ip_addr} = :inet_udp.getaddr(String.to_charlist(hostname))

      {:ok, socket} = @adapter.connect(ip_addr, port, [:binary])
      :inet.setopts(socket, active: false)
      {:ok, sharedkey} = handshake(socket, state)
      {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
      :ok = :inet.setopts(socket, active: true)

      new_state = Map.merge(state, %{socket: socket, sharedkey: sharedkey, tRef: tRef})
      Logger.debug("#{hostname}:#{port} | connected #{sharedkey}")
      IO.inspect(sharedkey, limit: :infinity)

      {:ok, check_mail_box(new_state)}
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

  defp check_mail_box(%{mailbox: []} = state), do: state

  defp check_mail_box(%{mailbox: mailbox, socket: socket, sharedkey: sharedkey} = state) do
    Enum.each(mailbox, fn {_key, msg} ->
      @adapter.send(socket, encode(msg, sharedkey))
    end)

    state
  end

  defp encode(msg, sharedkey) do
    bin = :erlang.term_to_binary(msg)
    iv = :crypto.strong_rand_bytes(@iv_bytes)

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
      <<iv::bytes-size(@iv_bytes), tag::bytes-size(@tag_bytes), ciphertext::binary>> = packet

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

  defp subscribe(vid) do
    PubSub.subscribe(@pubsub_server, "msg")
    PubSub.subscribe(@pubsub_server, "msg:#{vid}")
  end

  defp unsubscribe(vid) do
    PubSub.unsubscribe(@pubsub_server, "msg")
    PubSub.unsubscribe(@pubsub_server, "msg:#{vid}")
  end
end
