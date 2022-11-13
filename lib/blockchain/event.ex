defmodule Ipncore.Event do
  use Ecto.Schema
  alias Ipncore.{Address, Block, Chain, Tx, Wallet}
  import Ipnutils.Macros, only: [deftypes: 1, defstatus: 1]
  import Ecto.Query
  import Ipnutils.Filters
  alias __MODULE__

  deftypes do
    [
      {100, "tx.coinbase"},
      {101, "tx.send"},
      {102, "tx.refund"},
      {103, "tx.reward"},
      {104, "tx.jackpot"},
      {105, "tx.burned"},
      {106, "tx.withdrawal"},
      {200, "token.new"},
      {201, "token.update"},
      {202, "token.delete"},
      {300, "validator.new"},
      {301, "validator.update"},
      {302, "validator.delete"},
      {400, "domain.new"},
      {401, "domain.update"},
      {402, "domain.delete"},
      {410, "dns.new"},
      {411, "dns.delete"}
    ]
  else
    {false, false}
  end

  defstatus do
    [
      {0, "no processed"},
      {1, "pending"},
      {2, "approved"},
      {3, "complete"},
      {-1, "cancelled"},
      {-2, "timeout"}
    ]
  else
    {false, false}
  end

  @version Application.compile_env(:ipncore, :event_version)
  @threshold_timeout Application.compile_env(:ipncore, :event_threshold_timeout)
  @max_size Application.compile_env(:ipncore, :event_max_size)
  @max_signatures Application.compile_env(:ipncore, :event_max_signatures, 5)

  @base :ev
  @filaname "event.db"

  # mime
  # 0 - text
  # 1 - csv
  # 2 - json
  # 3 - xml
  # 4 - cbor

  @primary_key {:id, :binary, []}
  schema "event" do
    field(:time, :integer)
    field(:hash, :binary)
    field(:type, :integer)
    field(:block_index, :integer)
    field(:sig_count, :integer)
    field(:status, :integer, default: 1)
    field(:size, :integer, default: 0)
    field(:vsn, :integer)
  end

  def open(block_height) do
    dir_path = Application.get_env(:ipncore, :events_dir_path, "events")
    filename = Path.join(dir_path, "#{block_height}.db")
    DetsPlus.open_file(@base, name: filename, auto_save_memory: 1_000_000)
  end

  def close(block_height) do
    dir_path = Application.get_env(:ipncore, :events_dir_path, "events")
    DetsPlus.close(@base)
  end

  def new([version, type_number, channel, body, sigs, time], opts \\ [])
      when version == @version do
    # check_timeout = Keyword.has_key?(opts, :check_timeout)
    # if check_timeout and abs(time - Chain.get_time()) > @timeout, do: throw("Event is timeout")

    type_name = type_name(type_number)
    if type_name == false, do: throw("Type invalid")

    if length(sigs) > @max_signatures, do: throw("Invalid signature count")

    body_text = Jason.encode!(body)

    hash = calc_hash(version, channel, type_number, body_text, time)
    {from_addresses, sigs_size, sig_count} = check_signatures!(hash, sigs)

    size = byte_size(body_text) + sigs_size
    if size > @max_size, do: throw("Body size exceeded")

    next_index = Block.next_index(time)
    genesis_time = Chain.genesis_time()
    id = generate_id(next_index, genesis_time, hash, time)

    event = %{
      id: id,
      hash: hash,
      type: type_number,
      block_index: next_index,
      sig_count: sig_count,
      size: size,
      time: time,
      vsn: version
    }

    put!({id, [@version, channel, type_number, body, time]})

    try do
      multi = multi_insert(Ecto.Multi.new(), event, channel)

      result_multi =
        case type_name do
          "tx.send" ->
            [from_address, token, amount, to_address, validator_address, refundable, memo] = body
            [from_address] = from_addresses

            Tx.send!(
              multi,
              channel,
              event.id,
              event.time,
              event.size,
              token,
              from_address,
              amount,
              Address.from_text(to_address),
              Address.from_text(validator_address),
              refundable,
              memo
            )

          "tx.coinbase" ->
            [token_id, outputs, memo] = body
            [from_address] = from_addresses

            Tx.coinbase!(
              multi,
              channel,
              event.id,
              event.time,
              token_id,
              from_address,
              outputs,
              memo
            )

          "domain.new" ->
            [name, owner, email, avatar, validator_address] = body
            [from_address] = from_addresses

            Domain.new!(
              multi,
              channel,
              event.id,
              event.time,
              event.size,
              from_address,
              name,
              owner,
              email,
              avatar,
              Address.from_text(validator_address)
            )

          "domain.update" ->
            [name, validator_address, params] = body
            [owner_address] = from_addresses

            Domain.event_update!(
              multi,
              channel,
              event.id,
              event.time,
              event.size,
              owner_address,
              name,
              validator_address,
              params
            )

          "domain.delete" ->
            nil

          _ ->
            nil
        end

      case result_multi do
        nil ->
          nil

        result ->
          Repo.transaction(result_multi)
      end

      {:ok, event}
    catch
      _x ->
        delete(id)
        :error
    end
  end

  def new(_, _) do
    throw("Event not match")
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

  def generate_id(next_block_index, genesis_time, hash, time) do
    start_time = Block.block_index_start_time(next_block_index, genesis_time)

    [
      :binary.encode_unsigned(next_block_index),
      <<time - start_time::16>>,
      :binary.part(hash, 0, 8)
    ]
    |> IO.iodata_to_binary()
  end

  def calc_hash(vsn, channel, type_number, event_body_text, time) do
    [
      to_string(vsn),
      channel,
      to_string(type_number),
      event_body_text,
      to_string(time)
    ]
    |> IO.iodata_to_binary()
    |> Crypto.hash3()
  end

  def check_signatures!(hash, sigs) do
    {addresses, byte_size} =
      Enum.reduce(sigs, {[], 0}, fn [address, signature], {acc_addr, acc_size} ->
        bin_address = Address.from_text(address)

        case Wallet.get(bin_address) do
          nil ->
            throw("There is an unregistered address")

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

  def encode_id(index) do
    Base62.encode(index)
  end

  def decode_id(index) do
    index
    |> Base62.decode()
    |> ByteUtils.zeros_pad_leading(104)
  end

  def multi_insert(multi, name, event, channel) do
    Ecto.Multi.insert(multi, event, returning: false, prefix: channel)
  end

  defmacro map_select do
    quote do
      %{
        id: ev.id,
        time: ev.time,
        hash: fragment("encode(?, 'hex')", ev.hash),
        type: ev.type,
        block_index: ev.block_index,
        sig_count: ev.sig_count,
        status: ev.status,
        size: ev.size,
        vsn: ev.vsn
      }
    end
  end

  defmacro export_select do
    quote do
      [
        ev.vsn,
        ev.type,
        ev.body,
        ev.sigs,
        ev.time
      ]
    end
  end

  def fetch!(bin_id, channel) do
    from(ev in Event, where: ev.id == ^bin_id)
    |> Repo.one(prefix: channel)
    |> case do
      nil ->
        throw("Event no exists")

      ev ->
        ev
    end
  end

  def one(id, channel) do
    from(ev in Event, where: ev.id == ^id)
    |> Repo.one(prefix: channel)
  end

  def one_by_hash(hash, channel) do
    from(ev in Event, where: ev.hash == ^hash)
    |> Repo.one(prefix: channel)
  end

  def all(params) do
    from(ev in Event)
    |> filter_time(params)
    |> filter_offset(params)
    |> filter_limit(params, 10, 1000)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_time(query, %{"from" => from_time, "to" => to_time}) do
    where(query, [ev], ev.time >= ^from_time and ev.time <= ^to_time)
  end

  defp filter_time(query, %{"from" => from_time}) do
    where(query, [ev], ev.time >= ^from_time)
  end

  defp filter_time(query, %{"to" => to_time}) do
    where(query, [ev], ev.time <= ^to_time)
  end

  defp filter_time(query, _), do: query

  defp filter_select(query, %{"fmt" => "export"}) do
    select(query, [ev], export_select())
  end

  defp filter_select(query, _), do: select(query, [ev], map_select())

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [ev], asc: fragment("length(?)", ev.id), asc: ev.id)

      _ ->
        order_by(query, [ev], desc: fragment("length(?)", ev.id), desc: ev.id)
    end
  end

  defp filter_map(data) do
    Enum.map(data, fn x ->
      transform(x)
    end)
  end

  defp transform(nil), do: []
  defp transform([]), do: []

  defp transform(x) do
    %{x | id: encode_id(x.id), status: status_name(x.status), type: type_name(x.type)}
  end
end
