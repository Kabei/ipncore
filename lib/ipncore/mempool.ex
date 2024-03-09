defmodule Mempool do
  use GenServer
  # alias Phoenix.PubSub
  alias Ippan.Account
  require Logger

  @name :mempool
  # @pubsub :pubsub

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: __MODULE__)
  end

  @impl true
  def init(_) do
    :persistent_term.put(@name, self())
    cref = :counters.new(1, [])
    :persistent_term.put(:msg_counter, cref)

    load()

    {:ok,
     %{
       ets_msg: :ets.whereis(:msg),
       ets_hash: :ets.whereis(:hash),
       ets_dhash: :ets.whereis(:dhash)
     }}
  end

  def regular(body, returns) do
    pid = :persistent_term.get(@name)
    GenServer.call(pid, {:regular, body, returns}, :infinity)
  end

  def deferred(body, returns) do
    pid = :persistent_term.get(@name)
    GenServer.call(pid, {:deferred, body, returns}, :infinity)
  end

  def clear_cache do
    pid = :persistent_term.get(@name)
    GenServer.cast(pid, :clear)
  end

  @impl true
  def handle_call(
        {
          :regular,
          [hash, type, from, nonce, args, msg_sig, size],
          return
        },
        _from,
        state = %{ets_hash: ets_hash, ets_msg: ets_msg}
      ) do
    nonce_key = {from, nonce}

    result =
      case :ets.insert_new(ets_hash, {nonce_key, nil}) do
        true ->
          dets = DetsPlux.get(:nonce)
          cache = DetsPlux.tx(dets, :cache_nonce)

          # IO.puts("The nonce")

          case Account.update_nonce(dets, cache, from, nonce) do
            :error ->
              :ets.delete(ets_hash, nonce_key)
              {"error", "Invalid nonce x1"}

            _ ->
              # IO.puts("The check return")
              # IO.puts("The insert")
              cref = :persistent_term.get(:msg_counter)
              :counters.add(cref, 1, 1)
              ix = :counters.get(cref, 1)
              [_msg, sig] = msg_sig
              decode = [hash, type, from, nonce, args, sig, size]
              :ets.insert(ets_msg, {ix, 0, decode, msg_sig, return})

              # if ix == 1 do
              #   PubSub.local_broadcast(@pubsub, "block_timer", :check)
              # end

              # IO.puts("The result")
              %{"index" => ix}
          end

        false ->
          {"error", "Already exists (Core)"}
      end

    {:reply, result, state}
  end

  def handle_call(
        {:deferred, [hash, type, key, from, nonce | rest], return},
        _from,
        state = %{ets_dhash: ets_dhash, ets_hash: ets_hash, ets_msg: ets_msg}
      ) do
    nonce_key = {from, nonce}

    result =
      case :ets.insert_new(ets_hash, {nonce_key, nil}) do
        true ->
          msg_key = {type, key}

          dets = DetsPlux.get(:nonce)
          cache = DetsPlux.tx(dets, :cache_nonce)

          case :ets.insert_new(ets_dhash, {msg_key, nil}) do
            true ->
              [args, msg_sig, size] = rest

              # IO.puts("The nonce")

              case Account.update_nonce(dets, cache, from, nonce) do
                :error ->
                  :ets.delete(ets_hash, nonce_key)
                  :ets.delete(ets_dhash, msg_key)
                  {"error", "Invalid nonce x2"}

                _ ->
                  # IO.puts("The insert")
                  cref = :persistent_term.get(:msg_counter)
                  :counters.add(cref, 1, 1)
                  ix = :counters.get(cref, 1)
                  [_msg, sig] = msg_sig
                  decode = [hash, type, key, from, nonce, args, sig, size]
                  :ets.insert(ets_msg, {ix, 1, decode, msg_sig, return})

                  # if ix == 1 do
                  #   PubSub.local_broadcast(@pubsub, "block_timer", :check)
                  # end

                  # IO.puts("The result")
                  %{"index" => ix}
              end

            false ->
              :ets.delete(ets_hash, nonce_key)
              Account.revert_nonce(cache, from)
              {"error", "Deferred transaction already exists (Core)"}
          end

        false ->
          {"error", "Already exists (Core) #{inspect(nonce_key)}"}
      end

    {:reply, result, state}
  end

  @impl true
  def handle_cast(:clear, state = %{ets_msg: ets_msg}) do
    if :ets.info(ets_msg, :size) == 0 do
      cache_wallet_tx = DetsPlux.tx(:wallet, :cache_wallet)
      cache_balance_tx = DetsPlux.tx(:balance, :cache_balance)
      cache_nonce_tx = DetsPlux.tx(:nonce, :cache_nonce)
      cache_supply = DetsPlux.tx(:stats, :cache_supply)
      DetsPlux.clear_tx(cache_wallet_tx)
      DetsPlux.clear_tx(cache_balance_tx)
      DetsPlux.clear_tx(cache_nonce_tx)
      DetsPlux.clear_tx(cache_supply)
      cref = :persistent_term.get(:msg_counter)
      :counters.put(cref, 1, 0)
    end

    {:noreply, state}
  end

  @filename "mem.data"
  # Create and load mempool table and counter
  defp load do
    dir = :persistent_term.get(:save_dir)
    filepath = Path.join(dir, "mem.data")
    cref = :persistent_term.get(:msg_counter)

    if File.exists?(filepath) do
      {:ok, content} = File.read(filepath)

      case CBOR.Decoder.decode(content) do
        {%{"data" => data, "ix" => ix}, _rest} ->
          :ets.insert(:msg, data)
          :counters.put(cref, 1, ix)

        _other ->
          Logger.error("Error decode mem.data")
          {:error, :cbor_decoder_error}
      end

      File.rm(filepath)
    end
  end

  def save do
    size = :ets.info(:msg, :size)

    if size != 0 do
      dir = :persistent_term.get(:save_dir)
      filepath = Path.join(dir, @filename)
      data = :ets.tab2list(:msg)
      ix = :erlang.element(1, :lists.last(data))
      content = %{"msg" => data, "ix" => ix, "size" => size} |> CBOR.encode()
      File.write(filepath, content)
    end
  end
end
