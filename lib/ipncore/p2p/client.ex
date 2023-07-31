defmodule Ippan.P2P.Client do
  use GenServer
  import Ippan.P2P, only: [decode!: 2, encode: 2]
  alias Phoenix.PubSub
  alias Ippan.{Address, Block, P2P}
  require Global
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
      GenServer.start_link(@module, {hostname, port, pubkey, vid, privkey},
        hibernate_after: 5_000
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
       hostname: hostname,
       port: port,
       address: address,
       pubkey: pubkey,
       privkey: privkey,
       conn: false,
       mailbox: load_mailbox(vid)
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
      :exit, _m ->
        # IO.inspect(m)
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
        msg,
        %{conn: conn, id: vid, mailbox: mailbox} = state
      ) do
    # IO.inspect("send a msg #{inspect(msg)}")

    continue =
      case msg do
        {_, except} ->
          vid not in except

        _msg ->
          true
      end

    result =
      cond do
        continue == false ->
          state

        conn ->
          @adapter.send(state.socket, encode(msg, state.sharedkey))
          state

        true ->
          case msg do
            %{id: id} ->
              # IO.inspect("set in mailbox")
              save_mailbox(vid, id, msg)
              mailbox = Map.put(mailbox, id, msg)
              %{state | mailbox: mailbox}

            _ ->
              state
          end
      end

    {:noreply, result}
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
      {:ok, ip_addr} = :inet_udp.getaddr(String.to_charlist(hostname))

      {:ok, socket} = @adapter.connect(ip_addr, port, @tcp_opts)
      :inet.setopts(socket, active: false)
      {:ok, sharedkey} = handshake(socket, state)
      {:ok, tRef} = :timer.send_after(@ping_interval, :ping)
      :ok = :inet.setopts(socket, active: true)

      new_state =
        Map.merge(state, %{conn: true, socket: socket, sharedkey: sharedkey, tRef: tRef})

      Logger.debug("#{hostname}:#{port} | connected")

      {:ok, check_mailbox(new_state)}
    rescue
      _error ->
        {:error, state}
    end
  end

  defp handshake(socket, state) do
    case @adapter.recv(socket, 0, @handshake_timeout) do
      {:ok, "WEL" <> @version <> pubkey} ->
        {:ok, ciphertext, sharedkey} = NtruKem.enc(pubkey)

        id = Global.validator_id()
        {:ok, signature} = Cafezinho.Impl.sign(sharedkey, state.privkey)
        authtext = encode(state.pubkey <> <<id::64>> <> signature, sharedkey)
        @adapter.send(socket, "THX" <> ciphertext <> authtext)
        {:ok, sharedkey}

      error ->
        error
    end
  end

  defp check_mailbox(%{id: vid, mailbox: mailbox, socket: socket, sharedkey: sharedkey} = state) do
    # IO.inspect(mailbox)

    if mailbox != %{} do
      for {_, msg} <- mailbox do
        @adapter.send(socket, encode(msg, sharedkey))
      end

      data_dir = Application.get_env(:ipncore, :data_dir)
      file_path = Path.join(data_dir, "mailbox.#{vid}.tmp")
      File.rm(file_path)

      # IO.inspect("mailbox sent")
    else
      me = Global.validator_id()
      b1 = BlockStore.count(vid)
      b2 = BlockStore.count(me)

      if b2 > b1 do
        BlockStore.fetch_between(me, b1, b2)
        |> case do
          {:ok, data} ->
            for block <- data do
              P2P.push(vid, ["new_recv", Block.to_map(block)])
            end

          _ ->
            :ok
        end
      end
    end

    %{state | mailbox: %{}}
  end

  defp save_mailbox(vid, id, value) do
    data_dir = Application.get_env(:ipncore, :data_dir)
    file_path = Path.join(data_dir, "mailbox.#{vid}.tmp")
    value = :erlang.term_to_binary(value) |> Fast64.encode64()

    {:ok, file} = File.open(file_path, [:append])
    IO.write(file, "#{id}=#{value}\n")
    File.close(file)
  end

  defp load_mailbox(vid) do
    data_dir = Application.get_env(:ipncore, :data_dir)
    file_path = Path.join(data_dir, "mailbox.#{vid}.tmp")

    if File.exists?(file_path) do
      result =
        File.stream!(file_path, [], :line)
        |> Enum.map(fn text ->
          text
          |> String.trim()
          |> String.replace(~r/\n|\r/, "")
          |> String.split("=", parts: 2)
          |> case do
            [key, value] -> {key, Fast64.decode64(value) |> :erlang.binary_to_term()}
            _ -> :ignored
          end
        end)
        |> Enum.filter(fn
          :ignored -> false
          _ -> true
        end)
        |> Enum.into(%{})

      if result != %{} do
        File.rm(file_path)
      end

      result
    else
      %{}
    end
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
