defmodule Ipncore.Event do
  use Ecto.Schema
  alias Ipncore.{Address, Block, Chain, Domain, Repo, Token, Tx, Validator, Wallet}
  import Ipnutils.Macros, only: [deftypes: 1]
  # import Ipnutils.Macros, only: [deftypes: 1, defstatus: 1]
  import Ecto.Query
  import Ipnutils.Filters
  alias __MODULE__

  deftypes do
    [
      {100, "validator.new"},
      {101, "validator.update"},
      {102, "validator.delete"},
      {200, "token.new"},
      {201, "token.update"},
      {202, "token.delete"},
      {210, "tx.coinbase"},
      {211, "tx.send"},
      {211, "tx.sendmulti"},
      {212, "tx.refund"},
      {213, "tx.jackpot"},
      # only validators
      {214, "tx.reward"},
      {215, "tx.burned"},
      {400, "domain.new"},
      {401, "domain.update"},
      {402, "domain.delete"},
      {410, "dns.new"},
      {411, "dns.update"},
      {412, "dns.delete"},
      {1000, "pubkey.new"}
    ]
  else
    {false, false}
  end

  # defstatus do
  #   [
  #     {0, "no processed"},
  #     {1, "pending"},
  #     {2, "approved"},
  #     {3, "complete"},
  #     {-1, "cancelled"},
  #     {-2, "timeout"}
  #   ]
  # else
  #   {false, false}
  # end

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

  @type t :: %__MODULE__{
          id: binary(),
          time: pos_integer(),
          # hash: binary(),
          type: pos_integer(),
          block_index: pos_integer(),
          sig_count: pos_integer(),
          size: pos_integer(),
          vsn: pos_integer()
        }

  @primary_key {:id, :binary, []}
  schema "event" do
    field(:time, :integer)
    field(:type, :integer)
    field(:block_index, :integer)
    field(:sig_count, :integer)
    field(:size, :integer, default: 0)
    field(:vsn, :integer)
  end

  def open(block_height) do
    dir_path = Application.get_env(:ipncore, :events_path, "events")
    filename = Path.join(dir_path, "#{block_height}.db")
    DetsPlus.open_file(@base, name: filename, auto_save_memory: 1_000_000_000)
  end

  def close do
    DetsPlus.close(@base)
  end

  def version, do: @version

  def timeout, do: @threshold_timeout

  def check!(@version, "pubkey.new", time, body, address, signature)
      when is_binary(signature) and is_binary(body) and time > 0 do
    address_hash = Address.hash(body)
    pubkey = Base.decode64!(body)
    sig = Base.decode64!(signature)
    hash = calc_hash(1000, pubkey, time)

    Wallet.check!(hash, pubkey, signature)

    case Mempool.push!(hash, time, 1000, address_hash, pubkey, sig) do
      true ->
        :ok

      _ ->
        throw("Error push to mempool")
    end
  end

  def check!(@version, type_name, time, body, address, sig64) do
    type_number = type_index(type_name)
    if type_name == false, do: throw("Type invalid")
    body_text = Jason.encode!(body)

    hash = calc_hash(type_number, body_text, time)
    signature = Base.decode64!(sig64)

    size = byte_size(body_text) + byte_size(signature)
    if size > @max_size, do: throw("Body size exceeded")

    from_address = Address.from_text(address)
    pubkey = Wallet.get(from_address)

    if Falcon.verify(hash, signature, pubkey) == :error, do: throw("Invalid signature")

    case type_name do
      "token.new" ->
        token_id = List.first(body)
        Token.check_new!(token_id, from_address)

      "token.delete" ->
        token_id = List.first(body)
        Token.check_delete!(token_id, from_address)

      "validator.new" ->
        [hostname | _rest] = body
        Validator.check_new!(hostname)

      "validator.update" ->
        [hostname | _rest] = body
        Validator.check_update!(hostname, from_address)

      "validator.delete" ->
        [hostname | _rest] = body
        Validator.check_delete!(hostname, from_address)

      "domain.new" ->
        [name, _owner, email, avatar, validator_address] = body

        Domain.check_new!(
          name,
          from_address,
          email,
          avatar,
          Address.from_text(validator_address),
          size
        )

      "domain.udpate" ->
        [name | params] = body
        Domain.check_update!(name, from_address, validator_address, params)

      "domain.delete" ->
        [host, validator_address, params] = body
        Domain.check_delete!(name, from_address, Address.from_text(validator_address), params)

      "tx.send" ->
        [to_address, token, amount, validator_address] = body

        Tx.check_send!(
          from_address,
          Address.from_text(to_address),
          token,
          amount,
          Address.from_text(validator_address),
          size
        )

      "tx.coinbase" ->
        [token, outputs] = body
        Tx.check_coinbase!(from_address, token)
    end

    case Mempool.push!(hash, time, type_number, from_address, body, signature) do
      true ->
        :ok

      false ->
        throw("Error push to mempool")
    end
  end

  def new(hash, time, type_number, address, body, signature) do
    # check_timeout = Keyword.has_key?(opts, :check_timeout)
    # if check_timeout and abs(time - Chain.get_time()) > @timeout, do: throw("Event is timeout")

    put!({hash, time, @version, type_number, address, body, signature})

    # event = %{
    #   id: hash,
    #   type: type_number,
    #   block_index: next_index,
    #   sig_count: sig_count,
    #   size: size,
    #   time: time,
    #   vsn: version
    # }

    # case type_name do
    #   "pubkey.new" ->
    #     Wallet.put(body)
    # end

    # try do
    #   multi = multi_insert(Ecto.Multi.new(), event, channel)

    #   result_multi =
    #     case type_name do
    #       "tx.send" ->
    #         [from_address, token, amount, to_address, validator_address, refundable, memo] = body
    #         [from_address] = from_addresses

    #         Tx.send!(
    #           multi,
    #           channel,
    #           event.id,
    #           event.time,
    #           event.size,
    #           token,
    #           from_address,
    #           amount,
    #           Address.from_text(to_address),
    #           Address.from_text(validator_address),
    #           refundable,
    #           memo
    #         )

    #       "tx.coinbase" ->
    #         [token_id, outputs, memo] = body
    #         [from_address] = from_addresses

    #         Tx.coinbase!(
    #           multi,
    #           channel,
    #           event.id,
    #           event.time,
    #           token_id,
    #           from_address,
    #           outputs,
    #           memo
    #         )

    #       "domain.new" ->
    #         [name, owner, email, avatar, validator_address] = body
    #         [from_address] = from_addresses

    #         Domain.new!()

    #         Domain.new!(
    #           multi,
    #           channel,
    #           event.id,
    #           event.time,
    #           event.size,
    #           from_address,
    #           name,
    #           owner,
    #           email,
    #           avatar,
    #           Address.from_text(validator_address)
    #         )

    #       "domain.update" ->
    #         [name, validator_address, params] = body
    #         [owner_address] = from_addresses

    #         Domain.event_update!(
    #           multi,
    #           channel,
    #           event.id,
    #           event.time,
    #           event.size,
    #           owner_address,
    #           name,
    #           validator_address,
    #           params
    #         )

    #       "domain.delete" ->
    #         nil

    #       _ ->
    #         nil
    #     end

    #   case result_multi do
    #     nil ->
    #       nil

    #     result ->
    #       Repo.transaction(result_multi)
    #   end

    #   {:ok, event}
    # catch
    #   _x ->
    #     delete(id)
    #     :error
    # end
  end

  # def new(_, _) do
  #   throw("Event not match")
  # end

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

  def calc_hash(type_number, event_body_text, time) do
    [
      to_string(type_number),
      event_body_text,
      to_string(time)
    ]
    |> Crypto.hash3()
  end

  def calc_hash(event) do
    [
      to_string(event.vsn),
      to_string(event.type),
      event.body,
      to_string(event.time)
    ]
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

  # def encode_id(index) do
  #   Base62.encode(index)
  # end

  # def decode_id(index) do
  #   index
  #   |> Base62.decode()
  #   |> ByteUtils.zeros_pad_leading(104)
  # end

  defmacro map_select do
    quote do
      %{
        id: fragment("encode(?, 'hex')", ev.id),
        time: ev.time,
        type: ev.type,
        block_index: ev.block_index,
        sig_count: ev.sig_count,
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
    |> transform()
  end

  def one_by_hash(hash, channel) do
    from(ev in Event, where: ev.hash == ^hash)
    |> Repo.one(prefix: channel)
    |> transform()
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
    # %{x | id: encode_id(x.id), type: type_name(x.type)}
    %{x | type: type_name(x.type)}
  end
end
