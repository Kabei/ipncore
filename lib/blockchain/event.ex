defmodule Ipncore.Event do
  use Ecto.Schema
  alias Ipncore.{Block, Chain}
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
      {104, "tx.jacpot"},
      {105, "tx.burned"},
      {106, "tx.withdrawal"},
      {200, "token.new"},
      {201, "token.update"},
      {202, "token.delete"}
    ]
  else
    {false, false}
  end

  defstatus do
    [
      {0, "no processed"},
      {1, "pending"},
      {2, "approved"},
      {3, "confirmed"},
      {-1, "cancelled"},
      {-2, "timeout"}
    ]
  else
    {false, false}
  end

  @version Aplication.get_env(:ipncore, :event_version)
  @timeout Aplication.get_env(:ipncore, :event_timeout)
  @max_size Aplication.get_env(:ipncore, :event_max_size)
  @max_signatures Aplication.get_env(:ipncore, :event_max_signatures, 5)

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
    field(:status, :integer, default: 100)
    field(:size, :integer, default: 0)
    field(:vsn, :integer)
  end

  def open(block_height) do
    dir_path = Application.get_env(:ipncore, :events_dir_path, "events")
    filename = Path.join(dir_path, "#{block_height}.db")
    DetsPlus.open_file(:ev, name: filename)
  end

  def close(block_height) do
    dir_path = Application.get_env(:ipncore, :events_dir_path, "events")
    DetsPlus.close(:ev)
  end

  def new([version, type, channel, body, sigs, time], opts \\ [])
      when mime == 2 and version == @version do
    # check_timeout = Keyword.has_key?(opts, :check_timeout)
    # if check_timeout and abs(time - Chain.get_time()) > @timeout, do: throw("Event is timeout")

    type_name = type_name(type)
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
      type: type,
      block_index: next_index,
      sig_count: sig_count,
      size: size,
      time: time,
      vsn: version
    }

    insert_new!(id, [@version, channel, type_number, body, time])

    try do
      case type_name do
        "tx.send" ->
          Tx.send(channel, event, from_addresses, body, next_index, genesis_time)

        "tx.coinbase" ->
          Tx.coinbase(channel, event, from_addresses, body, next_index, genesis_time)
      end
    catch
      _x ->
        delete(id)
    end
  end

  def new(_) do
    throw(RuntimeError, "Event not match")
  end

  def insert_new!(tx, key, value) do
    case DetsPlus.insert_new(@base, key, value) do
      false ->
        throw(RuntimeError, "Event already exists")

      true ->
        true
    end
  end

  def delete(key) do
    DetsPlus.delete(@base, key)
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
    {addresses, size} =
      Enum.reduce({addresses, 0}, fn [address, signature], {acc_addr, acc_size} ->
        bin_address = Address.from_text(address)

        case Wallet.get(bin_address) do
          [{_, pubkey}] ->
            case Falcon.verify(hash, signature, pubkey) do
              :ok ->
                {acc_addr ++ [bin_address], byte_size(signature) + acc_size}

              :error ->
                throw("Error signature")
            end

          _ ->
            throw("There is an unregistered address")
        end
      end)

    {addresses, size, length(sigs)}
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

  defmacro export_select(channel) do
    quote do
      [
        ev.vsn,
        ev.type,
        channel,
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
    from(ev in Event, where: ev.id == ^decode_id(id))
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
    |> transform()
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
    select(query, [ev], list_select())
  end

  defp filter_select(query, _), do: select(query, [ev], map_select())

  defp transform(nil), do: []
  defp transform([]), do: []

  defp transform(data) do
    Enum.map(data, fn x ->
      transform_one(x)
    end)
  end

  defp transform_one(x) do
    %{x | id: encode_id(x.id), status: status_name(x.status), type: type_name(x.type)}
  end
end
