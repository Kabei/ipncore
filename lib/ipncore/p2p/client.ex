defmodule Ippan.P2P.Client do
  use GenServer
  import Ippan.P2P, only: [decode!: 2, encode: 2]
  alias Phoenix.PubSub
  alias Ippan.{Address, P2P}
  require Logger

  @module __MODULE__
  @adapter :gen_tcp
  @version P2P.version()
  @handshake_timeout P2P.handshake_timeout()
  @time_to_reconnect 3_000
  @ping_interval 45_000
  @pubsub_server :network
  @tcp_opts Application.compile_env(:ipncore, :p2p_client)

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
      GenServer.start_link(@module, {hostname, port, pubkey, vid, privkey}, hibernate_after: 5_000)

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
       hostname: hostname,
       port: port,
       address: address,
       pubkey: pubkey,
       privkey: privkey,
       conn: false,
       mailbox: %{}
     }, {:continue, :connect}}
  end

  def stop(pid) do
    GenServer.stop(pid, :normal)
  end

  @impl true
  def handle_continue(:connect, initial_state) do
    send(self(), :reconnect)
    {:noreply, initial_state}
  end

  @impl true
  def handle_info(:reconnect, state) do
    try do
      case connect(state) do
        {:ok, new_state} ->
          {:noreply, new_state}

        _error ->
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

  def handle_info(
        {:tcp, _socket, packet},
        %{sharedkey: sharedkey} = state
      ) do
    case decode!(packet, sharedkey) do
      %{id: id} = msg -> PubSub.local_broadcast(@pubsub_server, "res:#{id}", msg)
      _ -> :ok
    end

    {:noreply, state}
  end

  def handle_info(
        {:tcp_closed, _socket},
        %{hostname: hostname, socket: socket, port: port, tRef: tRef} = state
      ) do
    Logger.debug("tcp_closed | #{hostname}:#{port}")
    @adapter.close(socket)
    :timer.cancel(tRef)
    :timer.send_after(@time_to_reconnect, :reconnect)
    {:noreply, %{state | conn: false}}
  end

  if Mix.env() == :dev do
    def handle_info(
          {:tcp_error, _socket, reason},
          %{hostname: hostname, port: port} = state
        ) do
      Logger.debug("tcp_error #{reason} | #{hostname}:#{port}")
      {:noreply, state}
    end
  end

  def handle_info(:ping, %{socket: socket} = state) do
    @adapter.send(socket, "PING")
    {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
    {:noreply, Map.put(state, :tRef, tRef)}
  end

  def handle_info({:EXIT, _pid, {:exit_trap, reason}}, state) do
    {:stop, reason, state}
  end

  def handle_info({:mailbox, %{id: id} = msg}, %{mailbox: mailbox} = state) do
    {:noreply, %{state | mailbox: Map.put(mailbox, id, msg)}}
  end

  # receved a message from pubsub
  def handle_info(
        %{id: id} = msg,
        %{socket: socket, conn: conn, mailbox: mailbox, sharedkey: sharedkey} = state
      ) do
    case conn do
      true ->
        @adapter.send(socket, encode(msg, sharedkey))
        {:noreply, state}

      _ ->
        {:noreply, %{state | mailbox: Map.put(mailbox, id, msg)}}
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
      # Logger.debug("#{hostname}:#{port} | connecting...")

      {:ok, ip_addr} = :inet_udp.getaddr(String.to_charlist(hostname))

      {:ok, socket} = @adapter.connect(ip_addr, port, @tcp_opts)
      :inet.setopts(socket, active: false)
      {:ok, sharedkey} = handshake(socket, state)
      {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
      :ok = :inet.setopts(socket, active: true)

      new_state =
        Map.merge(state, %{socket: socket, sharedkey: sharedkey, tRef: tRef, conn: true})

      Logger.debug("#{hostname}:#{port} | connected")
      # IO.inspect(sharedkey, limit: :infinity)

      {:ok, check_mailbox(new_state)}
    rescue
      MatchError ->
        # {:reply, :error, state}
        {:error, state}
    end
  end

  defp handshake(socket, state) do
    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL" <> @version <> pubkey} ->
        # IO.inspect("Welcome #{byte_size(pubkey)}")
        {:ok, ciphertext, sharedkey} = NtruKem.enc(pubkey)

        id = Default.validator_id()
        {:ok, signature} = Cafezinho.Impl.sign(sharedkey, state.privkey)
        authtext = encode(state.pubkey <> <<id::64>> <> signature, sharedkey)
        @adapter.send(socket, "THX" <> ciphertext <> authtext)
        {:ok, sharedkey}

      error ->
        # IO.inspect("error")
        # IO.inspect(error)
        error
    end
  end

  defp check_mailbox(%{mailbox: %{}} = state), do: state

  defp check_mailbox(%{mailbox: mailbox, socket: socket, sharedkey: sharedkey} = state) do
    for {_, msg} <- mailbox do
      @adapter.send(socket, encode(msg, sharedkey))
    end

    %{state | mailbox: %{}}
  end

  defp subscribe(vid) do
    PubSub.subscribe(@pubsub_server, "echo")
    PubSub.subscribe(@pubsub_server, "echo:#{vid}")
  end

  defp unsubscribe(vid) do
    PubSub.unsubscribe(@pubsub_server, "echo")
    PubSub.unsubscribe(@pubsub_server, "echo:#{vid}")
  end
end
