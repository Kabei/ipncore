defmodule Ipncore.Event do
  use Ecto.Schema

  alias Ipncore.{
    Address,
    Balance,
    Domain,
    DnsRecord,
    Repo,
    RepoWorker,
    Token,
    Tx,
    Validator,
    Wallet
  }

  import Ipnutils.Macros, only: [deftypes: 1]
  import Ecto.Query
  import Ipnutils.Filters
  alias __MODULE__

  deftypes do
    [
      {1, "pubkey.new"},
      {100, "validator.new"},
      {101, "validator.update"},
      {102, "validator.delete"},
      {200, "token.new"},
      {201, "token.update"},
      {202, "token.delete"},
      {210, "tx.coinbase"},
      {211, "tx.send"},
      {212, "tx.sendmulti"},
      {213, "tx.refund"},
      {214, "tx.jackpot"},
      {215, "tx.reward"},
      {216, "tx.burn"},
      {250, "balance.lock"},
      {251, "balance.unlock"},
      {400, "domain.new"},
      {401, "domain.update"},
      {402, "domain.delete"},
      {403, "domain.renew"},
      {410, "dns.new"},
      {411, "dns.update"},
      {412, "dns.delete"}
    ]
  else
    {false, false}
  end

  @version Application.compile_env(:ipncore, :event_version)
  @threshold_timeout Application.compile_env(:ipncore, :event_threshold_timeout)
  @max_size Application.compile_env(:ipncore, :event_max_size)
  @imposible Default.imposible_address()
  @unit_time Default.unit_time()
  # @max_signatures Application.compile_env(:ipncore, :event_max_signatures, 5)

  @base :ev
  # @filaname "event.db"

  # mime
  # 0 - text
  # 1 - csv
  # 2 - json
  # 3 - xml
  # 4 - cbor
  @type t :: %__MODULE__{
          hash: binary(),
          time: pos_integer(),
          type: pos_integer(),
          block_index: pos_integer(),
          sig_count: pos_integer(),
          size: pos_integer(),
          vsn: pos_integer()
        }

  @primary_key {:hash, :binary, []}
  schema "event" do
    field(:time, :integer)
    field(:type, :integer)
    field(:block_index, :integer)
    field(:sig_count, :integer)
    field(:size, :integer, default: 0)
    field(:vsn, :integer)
  end

  def open(epoch_number) do
    dir_path = Application.get_env(:ipncore, :events_path, "data/events")
    File.mkdir_p(dir_path)
    filename = Path.join(dir_path, "#{epoch_number}.db")
    DetsPlus.open_file(@base, file: filename, auto_save_memory: 1_000_000_000, auto_save: 60_000)
  end

  def close do
    DetsPlus.close(@base)
  end

  def version, do: @version

  def timeout, do: @threshold_timeout

  @spec check(pos_integer, String.t(), pos_integer, term, String.t(), String.t()) ::
          {:ok, binary} | {:error, String.t()}
  def check(@version, "pubkey.new" = type_name, time, [pubkey64] = body, sig64) do
    try do
      type_number = type_index(type_name)
      if type_name == false, do: throw("Type invalid")
      body_text = Jason.encode!(body)
      hash = calc_hash(type_number, body_text, time)

      # check if already exists
      exists!(hash)

      signature = Base.decode64!(sig64)
      pubkey = Base.decode64!(pubkey64)
      size = byte_size(body_text) + byte_size(signature)
      from_address = Address.hash(pubkey)
      # IO.inspect("Address:")
      # IO.inspect(Address.to_text(from_address))
      # IO.inspect("Hash: #{Base.encode16(hash, case: :lower)}")

      if @imposible == from_address, do: throw("Imposible address")

      if Falcon.verify(hash, signature, pubkey) == :error, do: throw("Invalid signature")

      if Wallet.has_key?(from_address), do: throw("Pubkey already exists")

      Mempool.push!(time, hash, type_number, from_address, body, signature, size)
    rescue
      ex ->
        {:error, Exception.format(:error, ex, __STACKTRACE__)}
    catch
      x -> {:error, x}
    end
  end

  def check(@version, type_name, time, body, address, sig64) do
    try do
      # if check_timeout and abs(time - Chain.get_time()) > @timeout, do: throw("Event is timeout")

      type_number = type_index(type_name)
      if type_name == false, do: throw("Type invalid")

      body_text = Jason.encode!(body)

      from_address = Address.from_text!(address)
      hash = calc_hash(type_number, body_text, time)

      # check if already exists
      exists!(hash)
      signature = Base.decode64!(sig64)

      # IO.inspect(Base.encode16(hash, case: :lower))

      size = byte_size(body_text) + byte_size(signature)
      if size > @max_size, do: throw("Body size exceeded")

      pubkey = Wallet.fetch!(from_address)

      if Falcon.verify(hash, signature, pubkey) == :error, do: throw("Invalid signature")

      # new_body =
      case type_name do
        "token.new" ->
          token_id = List.first(body)
          Token.check_new!(token_id, from_address)

        "token.update" ->
          [token_id | _rest] = body
          Token.check_update!(token_id, from_address)

        "token.delete" ->
          token_id = List.first(body)
          Token.check_delete!(token_id, from_address)

        "validator.new" ->
          [hostname, name, _owner, avatar, fee_type, fee | _rest] = body
          Validator.check_new!(hostname, name, from_address, avatar, fee_type, fee)

        "validator.update" ->
          [hostname | _rest] = body
          Validator.check_update!(hostname, from_address)

        "validator.delete" ->
          [hostname] = body
          Validator.check_delete!(hostname, from_address)

        "tx.send" ->
          [token, to_address, amount, validator_host | _rest] = body

          Tx.check_send!(
            from_address,
            Address.from_text!(to_address),
            token,
            amount,
            validator_host,
            size
          )

        "tx.coinbase" ->
          [token, _outputs, note] = body
          Tx.check_coinbase!(from_address, token, note)

        "tx.refund" ->
          [tx_time, tx_hash] = body
          Tx.check_refund!(from_address, tx_time, decode_id(tx_hash))

        "balance.lock" ->
          [to_address, token_id, value] = body
          Balance.check_lock!(from_address, Address.from_text!(to_address), token_id, value)

        "domain.new" ->
          [name, email, avatar, title, years_to_renew, validator_host] = body

          Domain.check_new!(
            name,
            from_address,
            email,
            avatar,
            title,
            years_to_renew,
            validator_host,
            size
          )

        "domain.update" ->
          [host, _validator_host, _params] = body
          Domain.check_update!(host, from_address)

        "domain.delete" ->
          [host] = body
          Domain.check_delete!(host, from_address)

        "domain.renew" ->
          [host, years_to_renew, validator_host] = body

          Domain.check_renew!(
            host,
            from_address,
            years_to_renew,
            validator_host,
            time,
            size
          )

        "dns.new" ->
          [name, type, data, ttl | _validator_host] = body
          DnsRecord.check_new!(name, type, data, ttl, from_address)

        "dns.update" ->
          [name, type, old_hash_index, data, ttl | _validator_host] = body
          DnsRecord.check_update!(name, type, old_hash_index, data, ttl, from_address)

        "dns.delete" ->
          DnsRecord.check_delete!(body, from_address)

        _ ->
          throw("Invalid Match Type")
      end

      Mempool.push!(time, hash, type_number, from_address, body, signature, size)
    rescue
      ex ->
        {:error, Exception.format(:error, ex, __STACKTRACE__)}
    catch
      x -> {:error, x}
    end
  end

  @spec new!(pos_integer, binary, pos_integer, pos_integer, binary, term, binary, pos_integer) ::
          Event.t()
  def new!(next_index, hash, time, type_number, from_address, body, signature, size) do
    type = type_name(type_number)

    event = %{
      hash: hash,
      type: type_number,
      block_index: next_index,
      sig_count: 1,
      size: size,
      time: time,
      vsn: @version
    }

    channel = Default.channel()

    multi =
      Ecto.Multi.new()
      |> multi_insert(event, channel)

    result =
      case type do
        "pubkey.new" ->
          body
          |> List.first()
          |> Base.decode64!()
          |> Wallet.put!()

          multi

        "token.new" ->
          [token_id, owner, name, decimals, symbol, avatar, props] = body

          Token.new!(
            multi,
            token_id,
            from_address,
            Address.from_text!(owner),
            name,
            decimals,
            symbol,
            avatar,
            props,
            time,
            channel
          )

        "token.update" ->
          [token_id, params] = body
          Token.event_update!(multi, from_address, token_id, params, time, channel)

        "token.delete" ->
          [token_id] = body
          Token.event_delete!(multi, token_id, from_address, channel)

        "validator.new" ->
          [hostname, name, owner, avatar, fee_type, fee] = body

          Validator.new!(
            multi,
            from_address,
            hostname,
            name,
            Address.from_text!(owner),
            avatar,
            fee_type,
            fee,
            time,
            channel
          )

        "validator.update" ->
          [hostname, params] = body

          Validator.event_update!(
            multi,
            from_address,
            hostname,
            params,
            time,
            channel
          )

        "validator.delete" ->
          [hostname] = body
          Validator.event_delete!(multi, hostname, from_address, channel)

        "tx.send" ->
          [token, to_address, amount, validator_host, note] = body

          Tx.send!(
            multi,
            hash,
            token,
            from_address,
            Address.from_text!(to_address),
            amount,
            validator_host,
            size,
            note,
            false,
            time,
            channel
          )

        "tx.coinbase" ->
          [token, outputs, note] = body
          Tx.coinbase!(multi, hash, token, from_address, outputs, note, time, channel)

        "tx.refund" ->
          [tx_time, tx_hash] = body
          Tx.refund!(multi, hash, from_address, tx_time, decode_id(tx_hash), size, time, channel)

        "balance.lock" ->
          [to_address, token_id, value] = body
          Balance.lock!(multi, Address.from_text!(to_address), token_id, value, time, channel)

        "domain.new" ->
          [name, email, avatar, title, years_to_renew, validator_host] = body

          Domain.new!(
            multi,
            hash,
            from_address,
            name,
            email,
            avatar,
            title,
            years_to_renew,
            validator_host,
            size,
            time,
            channel
          )

        "domain.update" ->
          [name, validator_host, params] = body

          Domain.event_update!(
            multi,
            hash,
            name,
            from_address,
            validator_host,
            params,
            time,
            channel
          )

        "domain.delete" ->
          [host] = body
          Domain.event_delete!(multi, host, from_address, channel)

        "domain.renew" ->
          [host, years_to_renew, validator_host] = body

          Domain.event_renew!(
            multi,
            hash,
            from_address,
            host,
            years_to_renew,
            validator_host,
            size,
            time,
            channel
          )

        "dns.new" ->
          [hostname, type, data, ttl, validator_host] = body

          DnsRecord.event_new!(
            multi,
            hash,
            from_address,
            hostname,
            type,
            data,
            ttl,
            validator_host,
            time,
            channel
          )

        "dns.update" ->
          [hostname, type, old_hash_index, data, ttl, validator_host] = body

          DnsRecord.event_update!(
            multi,
            hash,
            from_address,
            hostname,
            type,
            old_hash_index,
            data,
            ttl,
            validator_host,
            time,
            channel
          )

        "dns.delete" ->
          DnsRecord.event_delete!(multi, body, channel)
      end

    put!({hash, time, next_index, @version, type_number, from_address, body, signature})

    case result do
      :ok ->
        :ok

      multi_or_fun ->
        :ok
        # RepoWorker.run(multi_or_fun)
    end

    event
  end

  def put!(x) do
    case DetsPlus.insert_new(@base, x) do
      false ->
        throw("Event already exists")

      true ->
        true
    end
  end

  def delete(key) do
    DetsPlus.delete(@base, key)
  end

  def multi_insert(multi, event, channel) do
    Ecto.Multi.insert_all(multi, :event, Event, [event],
      returning: false,
      prefix: channel
    )
  end

  # def generate_id(next_block_index, genesis_time, hash, time) do
  #   start_time = Block.block_index_start_time(next_block_index, genesis_time)

  #   [
  #     :binary.encode_unsigned(next_block_index),
  #     <<time - start_time::16>>,
  #     :binary.part(hash, 0, 8)
  #   ]
  #   |> IO.iodata_to_binary()
  # end

  def calc_hash(type_number, event_body_text, time) when is_binary(event_body_text) do
    [
      to_string(type_number),
      event_body_text,
      to_string(time)
    ]
    |> Crypto.hash3()
  end

  def calc_hash(type_number, event_body_term, time) do
    [
      to_string(type_number),
      Jason.encode!(event_body_term),
      to_string(time)
    ]
    |> Crypto.hash3()
  end

  def check_signatures!(hash, sigs) do
    {addresses, byte_size} =
      Enum.reduce(sigs, {[], 0}, fn [address, signature], {acc_addr, acc_size} ->
        bin_address = Address.from_text!(address)

        case Wallet.get(bin_address) do
          nil ->
            throw("There is an unregistered pubkey")

          pubkey ->
            case Falcon.verify(hash, signature, pubkey) do
              :ok ->
                {acc_addr ++ [bin_address], byte_size(signature) + acc_size}

              :error ->
                throw("Error signature")
            end
        end
      end)

    {addresses, byte_size, length(sigs)}
  end

  # def encode_id(index) do
  #   Base62.encode(index)
  # end

  # def decode_id(index) do
  #   index
  #   |> Base62.decode()
  #   |> ByteUtils.zeros_pad_leading(104)
  # end

  defmacrop map_select do
    quote do
      %{
        hash: fragment("encode(?, 'hex')", ev.hash),
        time: ev.time,
        type: ev.type,
        block_index: ev.block_index,
        sig_count: ev.sig_count,
        size: ev.size,
        vsn: ev.vsn
      }
    end
  end

  # defmacro export_select do
  #   quote do
  #     [
  #       ev.vsn,
  #       ev.type,
  #       ev.body,
  #       ev.sigs,
  #       ev.time
  #     ]
  #   end
  # end

  def lookup(x) do
    DetsPlus.lookup(@base, x)
  end

  def lookup(hash, timestamp) do
    case DetsPlus.lookup(@base, hash) do
      [{_hash, time, _block_index, _version, _type_number, _from, _body, _signature}] = event ->
        cond do
          time == timestamp ->
            event

          true ->
            nil
        end

      [] ->
        nil
    end
  end

  def exists!(x) do
    case DetsPlus.member?(@base, x) do
      false ->
        false

      _ ->
        throw("Event already exists")
    end
  end

  def fetch_count(channel) do
    if Regex.match?(Const.Regex.channel(), channel) do
      %{rows: [[count]]} =
        Ecto.Adapters.SQL.query!(
          Ipncore.Repo,
          "SELECT n_live_tup FROM pg_stat_user_tables WHERE schemaname = $1 AND relname = $2 LIMIT 1",
          [channel, "event"]
        )

      count
    else
      0
    end
  end

  def one(hash, channel) do
    from(ev in Event, where: ev.hash == ^hash)
    |> select([ev], map_select())
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def one(hash, time, channel) do
    from(ev in Event, where: ev.hash == ^hash and ev.time == ^time)
    |> select([ev], map_select())
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def all(params) do
    from(ev in Event)
    |> filter_block(params)
    |> filter_type(params)
    |> filter_date(params)
    |> filter_offset(params)
    |> filter_limit(params)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_block(query, %{"block" => block}) do
    where(query, [ev], ev.block_index == ^block)
  end

  defp filter_block(query, _), do: query

  defp filter_type(query, %{"type" => type_name}) do
    type_number = type_index(type_name)
    where(query, [ev], ev.type == ^type_number)
  end

  defp filter_type(query, _), do: query

  def filter_date(query, %{"dateStart" => start_date, "dateEnd" => end_date}) do
    date_start = Utils.from_date_to_time(start_date, @unit_time)

    date_end = Utils.from_date_to_time(end_date, @unit_time)

    where(query, [ev], ev.time >= ^date_start and ev.time <= ^date_end)
  end

  def filter_date(query, %{"dateStart" => start_date}) do
    date_start = Utils.from_date_to_time(start_date, @unit_time)

    where(query, [ev], ev.time >= ^date_start)
  end

  def filter_date(query, %{"dateEnd" => end_date}) do
    date_end = Utils.from_date_to_time(end_date, @unit_time)

    where(query, [ev], ev.time <= ^date_end)
  end

  def filter_date(query, _), do: query

  defp filter_select(query, _), do: select(query, [ev], map_select())

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [ev], asc: ev.block_index, asc: ev.time, desc: ev.hash)

      _ ->
        order_by(query, [ev], desc: ev.block_index, desc: ev.time, asc: ev.hash)
    end
  end

  defp filter_map(data) do
    Enum.map(data, fn x ->
      transform(x)
    end)
  end

  def encode_id(id) do
    Base.encode16(id, case: :lower)
  end

  def decode_id(id) do
    Base.decode16!(id, case: :mixed)
  end

  defp transform(nil), do: nil
  defp transform([]), do: []

  defp transform(x) do
    %{x | type: type_name(x.type)}
  end
end
