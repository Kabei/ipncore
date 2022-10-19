defmodule Ipncore.Tx do
  use Ecto.Schema
  require Logger
  import Ipnutils.Macros, only: [deftypes: 1, defstatus: 1]
  import Ecto.Query, only: [from: 1, from: 2, where: 3, select: 3, order_by: 3, join: 5]
  import Ipnutils.Filters

  alias Ipncore.{
    Block,
    Chain,
    Channel,
    Migration,
    Txo,
    Txi,
    Repo,
    Utxo,
    Balance,
    Token,
    TxData,
    TxVote,
    Pool
  }

  alias Ipnutils.Address
  alias __MODULE__

  # unit time
  @unit_time :millisecond
  # tx version
  @version Application.get_env(:ipncore, :tx_version)
  # default token
  @token Default.token()
  # tx status
  @status_pending 100
  @status_approved 200
  @status_complete 201
  @status_cancelled 400
  @status_timeout 410
  # timeouts
  @timeout Application.get_env(:ipncore, :tx_timeout)
  @timeout_refund Application.get_env(:ipncore, :tx_timeout_refund)
  # max size
  @base_size 96
  @max_inputs 1024
  @max_outputs 16_000_000
  @max_memo_size 255
  @max_data_size 4096

  # mime type
  @mime_cbor "CBOR"
  @mime_text "TEXT"

  # output types
  @output_type_fee "%"
  @output_type_return "R"

  @type tx_type :: 0 | 1 | 2 | 3 | 4 | 5 | 100 | 101 | 102 | 103 | 200 | 201 | 300 | 301 | 1000
  @type tx_status :: 100 | 200 | 201 | 400

  @type t :: %__MODULE__{
          index: binary(),
          hash: binary(),
          block_index: pos_integer() | nil,
          sigs: [binary],
          type: tx_type(),
          status: tx_status(),
          memo: boolean(),
          amount: pos_integer(),
          total_input: pos_integer(),
          fees: pos_integer(),
          size: pos_integer(),
          vsn: pos_integer(),
          in_count: pos_integer(),
          out_count: pos_integer(),
          time: pos_integer(),
          outputs: [Txo],
          inputs: [Txi] | [] | nil
        }

  deftypes do
    [
      {0, "coinbase"},
      {1, "jackpot"},
      {2, "greate jackpot"},
      {3, "payconnect"},
      {4, "paystream"},
      {100, "regular"},
      {101, "from request"},
      {102, "non refundable"},
      {103, "gift"},
      {200, "free"},
      {201, "refund"},
      {300, "burned"},
      {301, "withdrawal"},
      {1000, "info"},
      {1001, "channel register"},
      {1002, "channel update"},
      {1011, "token_new"},
      {1012, "token_update"},
      {1013, "token_delete"},
      {1100, "dns registry"},
      {1101, "dns update"},
      {1102, "dns delete"},
      {1110, "pool_new"},
      {1111, "pool_update"},
      {1112, "pool_delete"}
    ]
  else
    # {100, "regular"}
    {false, false}
  end

  defstatus do
    [
      {100, "pending"},
      {200, "approved"},
      {201, "confirmed"},
      {400, "cancelled"},
      {401, "timeout"}
    ]
  else
    # {100, "pending"}
    {false, false}
  end

  @spec valid_size?(type :: tx_type(), size :: pos_integer()) :: boolean()
  # def valid_size?(0..99, _size), do: true
  def valid_size?(0..9, size) when size > 33_554_432, do: false
  def valid_size?(type, size) when type in 100..1999 and size > 16_384, do: false
  def valid_size?(_type, _size), do: true

  @primary_key {:index, :binary, []}
  schema "txs" do
    field(:hash, :binary)
    field(:type, :integer, default: 1)
    field(:status, :integer, default: @status_pending)
    field(:sigs, {:array, :binary})
    field(:amount, :integer)
    field(:memo, :boolean, default: false)
    field(:vsn, :integer, default: @version)
    field(:fees, :integer, default: 0)
    field(:size, :integer, default: 0)
    field(:in_count, :integer, default: 0)
    field(:out_count, :integer)
    field(:total_input, :integer, default: 0)
    field(:time, :integer)
    field(:block_index, :integer)
    field(:outputs, {:array, :map}, virtual: true)
    field(:inputs, {:array, :map}, virtual: true)
  end

  def version, do: @version
  def timeout, do: @timeout

  defmacro map_select do
    quote do
      %{
        amount: tx.amount,
        block_index: tx.block_index,
        fees: tx.fees,
        memo: txd.data,
        mime: txd.mime,
        hash: fragment("encode(?, 'hex')", tx.hash),
        in_count: tx.in_count,
        index: tx.index,
        out_count: tx.out_count,
        size: tx.size,
        sig_count: fragment("array_length(coalesce(?, '{}'::bytea[]), 1)", tx.sigs),
        status: tx.status,
        time: tx.time,
        total_input: tx.total_input,
        type: tx.type,
        vsn: tx.vsn
      }
    end
  end

  @spec new(tx_type(), [Txo.t()], pos_integer()) :: {t, [Txo.t()]}
  def new(type, outputs, amount) when type in 0..5 do
    time = Chain.get_time()
    genesis_time = Chain.genesis_time()
    block_index = Block.next_index(time, genesis_time)

    tx =
      %Tx{
        type: type,
        status: @status_complete,
        outputs: outputs,
        block_index: block_index,
        inputs: [],
        out_count: length(outputs),
        amount: amount,
        time: time
      }
      |> put_hash()
      |> put_index(genesis_time)
      |> from_struct()

    {tx, Txo.order(outputs, tx.index)}
  end

  @spec new(tx_type(), List.t(), List.t(), pos_integer(), pos_integer()) :: t
  def new(type, inputs, outputs, amount, fees)
      when length(inputs) > 0 and length(outputs) > 0 do
    time = Chain.get_time()
    genesis_time = Chain.genesis_time()
    block_index = Block.next_index(time, genesis_time)

    %Tx{
      type: type,
      inputs: inputs,
      outputs: outputs,
      block_index: block_index,
      in_count: length(inputs),
      out_count: length(outputs),
      amount: amount,
      fees: fees,
      total_input: Txo.compute_sum(inputs),
      time: time
    }
    |> put_hash()
    |> put_index(genesis_time)
    |> put_outputs_index()
  end

  @spec put_hash(Tx.t()) :: t
  def put_hash(tx) do
    Map.put(tx, :hash, compute_hash(tx))
  end

  @spec put_hash(Tx.t()) :: t
  def put_hash(tx, data) do
    Map.put(tx, :hash, compute_hash(tx, data))
  end

  @spec compute_hash(t, binary) :: binary()
  def compute_hash(tx, tx_data \\ <<>>) do
    r =
      [
        tx_data,
        Enum.reduce(Utils.array_normalize(Map.get(tx, :inputs, [])) ++ tx.outputs, "", fn x,
                                                                                          acc ->
          case x do
            %{address: address, tid: token, value: value} ->
              [
                acc,
                address,
                token,
                :binary.encode_unsigned(value)
              ]
              |> IO.iodata_to_binary()

            %{oid: oid} ->
              [
                acc,
                oid
              ]
              |> IO.iodata_to_binary()

            # used by tx builder
            oid when is_binary(oid) ->
              [
                acc,
                oid
              ]
              |> IO.iodata_to_binary()
          end
        end),
        :binary.encode_unsigned(tx.time)
      ]
      |> IO.iodata_to_binary()

    IO.inspect("pre hash msg")
    IO.inspect(r, limit: :infinity)
    IO.inspect(Base.encode16(r, case: :lower))

    r
    |> Crypto.hash3()
  end

  @spec is_coinbase?(t) :: boolean()
  def is_coinbase?(tx) do
    tx.type in 0..5
  end

  def encode_index(index) do
    Base62.encode(index)
  end

  def decode_index(index) do
    index
    |> Base62.decode()
    |> ByteUtils.zeros_pad_leading(72)
  end

  @spec generate_index(Tx.t()) :: binary()
  def generate_index(tx) do
    genesis_time = Chain.genesis_time()
    start_time = Block.block_index_start_time(tx.block_index, genesis_time)

    [
      :binary.encode_unsigned(tx.block_index),
      <<tx.time - start_time::32>>,
      :binary.part(tx.hash, 0, 4)
    ]
    |> IO.iodata_to_binary()
  end

  @spec generate_index(Tx.t(), pos_integer()) :: binary()
  def generate_index(tx, genesis_time) do
    start_time = Block.block_index_start_time(tx.block_index, genesis_time)

    [
      :binary.encode_unsigned(tx.block_index),
      <<tx.time - start_time::32>>,
      :binary.part(tx.hash, 0, 4)
    ]
    |> IO.iodata_to_binary()
    |> String.trim_leading(<<0>>)
  end

  def generate_index(block_index, timestamp, tx_hash, genesis_time) do
    start_time = Block.block_index_start_time(block_index, genesis_time)

    [
      :binary.encode_unsigned(block_index),
      <<timestamp - start_time::32>>,
      :binary.part(tx_hash, 0, 4)
    ]
    |> IO.iodata_to_binary()
    |> String.trim_leading(<<0>>)
  end

  defp put_index(tx, genesis_time) do
    %{tx | index: generate_index(tx, genesis_time)}
  end

  defp put_outputs_index(tx) do
    %{tx | outputs: Txo.order(tx.outputs, tx.index)}
  end

  defp put_inputs_index(tx) do
    Map.put(tx, :inputs, Enum.map(tx.inputs, &Map.put(&1, :txid, tx.index)))
  end

  defp put_size(tx) do
    tx_size = calc_size(tx.type, tx)
    unless valid_size?(tx.type, tx_size), do: throw(40208)

    %{tx | size: tx_size}
  end

  defp put_size(tx, data) do
    tx_size = calc_size(tx.type, tx, data)
    unless valid_size?(tx.type, tx_size), do: throw(40208)

    %{tx | size: tx_size}
  end

  defp put_fees(tx, utxo_address, pool_address, pool_fee, pool_percent) do
    {tx_amount, fees_amount, _retuned_amount} =
      extract_amounts!(tx.outputs, utxo_address, pool_address)

    fees = calc_fees(tx_amount, pool_fee, pool_percent, tx.size)

    IO.inspect("tx_size: #{tx.size}")
    IO.inspect("fees #{fees}")
    IO.inspect("amount #{tx_amount}")
    IO.inspect("fees_amount #{fees_amount}")

    if fees_amount != fees, do: throw(40209)

    %{tx | fees: fees, amount: tx_amount}
  end

  defp put_signatures(tx, sigs, utxo_address) do
    IO.inspect(tx)
    IO.inspect("hash: #{Base.encode16(tx.hash, case: :lower)}")
    signatures = extract_sigs_b64!(tx.hash, sigs, utxo_address)

    %{tx | sigs: signatures}
  end

  # def put_outputs_index(tx_index, outputs) do
  #   Txo.order(outputs, tx_index)
  # end

  @spec put_bft(binary, list, binary) :: :ok | :error
  def put_bft(txid, "timeout", channel) do
    try do
      {1, _} =
        from(tx in Tx, where: tx.index == ^txid)
        |> Repo.update_all([set: [status: @status_timeout]], prefix: channel)

      :ok
    rescue
      [Postgrex.Error, MatchError, CaseClauseError] ->
        :error
    end
  end

  def put_bft(txid, votes, channel) do
    try do
      Repo.transaction(fn ->
        status =
          case TxVote.build(votes) do
            :ok ->
              @status_approved

            _ ->
              @status_cancelled
          end

        # set tx status
        {1, _} =
          from(tx in Tx, where: tx.index == ^txid)
          |> Repo.update_all([set: [status: status]], prefix: channel)
      end)
      |> case do
        :ok ->
          :ok

        _ ->
          :error
      end
    rescue
      [Postgrex.Error, MatchError, CaseClauseError] ->
        :error
    catch
      x ->
        IO.inspect(x)
        :error
    end
  end

  @spec processing(Map.t()) :: {:ok, t} | {:error, atom()}
  def processing(%{
        "id" => channel_id,
        "pubkey" => channel_pubkey64,
        "sig" => sig,
        "time" => time,
        "type" => "channel register" = type_name,
        "version" => version
      }) do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      if @version != version, do: throw(40200)
      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)
      unless Channel.check_name?(channel_id), do: throw(40230)
      if Channel.exists?(channel_id), do: throw(40231)

      next_index = Block.next_index(time)
      genesis_time = Chain.genesis_time()
      owner_pubkey = PlatformOwner.pubkey()

      channel_pubkey = Base.decode64!(channel_pubkey64)
      signature = Base.decode64!(sig)

      channel = %{
        "id" => channel_id,
        "pubkey" => channel_pubkey,
        "time" => time
      }

      channel_data =
        channel
        |> CBOR.encode()

      if byte_size(channel_data) > @max_data_size, do: throw(40232)

      tx =
        %Tx{
          block_index: next_index,
          out_count: 0,
          amount: 0,
          sigs: [signature],
          time: time,
          type: type,
          status: @status_approved,
          vsn: version,
          memo: true,
          inputs: [],
          outputs: []
        }
        |> put_hash(channel_data)
        |> put_index(genesis_time)
        |> put_size(channel_data)

      if Falcon.verify(tx.hash, signature, owner_pubkey) != :ok, do: throw(40212)

      Ecto.Multi.new()
      |> Channel.multi_insert(:channel, channel)
      |> Ecto.Multi.run(:schema, fn _repo, _ ->
        unless Repo.schema_exists?(channel_id) do
          Migration.Blockchain.build(%{"channel" => channel_id, "version" => version})
          Chain.initialize(channel_id)
        end
        |> case do
          :ok ->
            {:ok, nil}

          _ ->
            {:error, nil}
        end
      end)
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        returning: false,
        prefix: channel_id
      )
      |> TxData.multi_insert(:txdata, tx.index, channel_data, @mime_cbor, Default.channel())
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  # new token
  def processing(%{
        "channel" => channel_id,
        "sig" => sig,
        "token" =>
          %{
            "id" => token_id,
            "name" => _token_name,
            "decimals" => token_decimal,
            "creator" => creator58,
            "owner" => owner58,
            "props" => %{
              "symbol" => token_symbol
            }
          } = token,
        "time" => time,
        "type" => "token_new" = type_name,
        "version" => version
      }) do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      if @version != version, do: throw(40200)
      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)
      unless Token.coin?(token_id), do: throw(40222)
      unless Channel.check_name?(channel_id), do: throw(40230)
      unless String.valid?(token_symbol), do: throw(40233)

      unless is_integer(token_decimal) or token_decimal > 10 or token_decimal < 0,
        do: throw(40234)

      next_index = Block.next_index(time)

      genesis_time = Chain.genesis_time()

      creator = Base58Check.decode(creator58)
      owner = Base58Check.decode(owner58)

      signature = Base.decode64!(sig)

      token =
        token
        |> Map.put("creator", creator)
        |> Map.put("owner", owner)

      data =
        token
        |> Token.filter_data()
        |> CBOR.encode()

      if byte_size(data) > @max_data_size, do: throw(40232)

      tx =
        %Tx{
          block_index: next_index,
          out_count: 0,
          amount: 0,
          sigs: [signature],
          time: time,
          type: type,
          status: @status_approved,
          vsn: version,
          memo: true,
          inputs: [],
          outputs: []
        }
        |> put_hash(data)
        |> put_index(genesis_time)
        |> put_size(data)

      if Falcon.verify(tx.hash, signature, PlatformOwner.pubkey()) == :error, do: throw(40212)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        returning: false,
        prefix: channel_id
      )
      |> TxData.multi_insert(:txdata, tx.index, data, @mime_cbor, channel_id)
      |> Token.multi_insert(:token, token, time, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  # create pool
  def processing(%{
        "channel" => channel_id,
        "time" => time,
        "pool" =>
          %{
            "hostname" => _hostname,
            "address" => address58,
            "fee" => _fee,
            "percent" => _percent
          } = pool,
        "type" => "pool_new" = type_name,
        "sig" => sig64,
        "version" => version
      }) do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      if @version != version, do: throw(40200)
      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)
      unless Channel.check_name?(channel_id), do: throw(40230)

      next_index = Block.next_index(time)
      genesis_time = Chain.genesis_time()
      signature = Base.decode64!(sig64)
      address = Base58Check.decode(address58)

      pool =
        pool
        |> Map.put("address", address)

      data =
        pool
        |> Pool.filter_data()
        |> CBOR.encode()

      if byte_size(data) > @max_data_size, do: throw(40232)

      tx =
        %Tx{
          block_index: next_index,
          out_count: 0,
          amount: 0,
          sigs: [signature],
          time: time,
          type: type,
          status: @status_approved,
          vsn: version,
          memo: true,
          inputs: [],
          outputs: []
        }
        |> put_hash(data)
        |> put_index(genesis_time)
        |> put_size(data)

      if Falcon.verify(tx.hash, signature, PlatformOwner.pubkey()) == :error, do: throw(40212)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        returning: false,
        prefix: channel_id
      )
      |> TxData.multi_insert(:txdata, tx.index, data, @mime_cbor, channel_id)
      |> Pool.multi_insert(:pool, pool, time, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  # update pool
  def processing(%{
        "channel" => channel_id,
        "sig" => sig64,
        "hostname" => hostname,
        "pool" => pool_params,
        "pubkey" => pubkey64,
        "time" => time,
        "type" => "pool_update" = type_name,
        "version" => version
      }) do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      if @version != version, do: throw(40200)

      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)
      unless Channel.check_name?(channel_id), do: throw(40230)

      pubkey = Base.decode64!(pubkey64)
      signature = Base.decode64!(sig64)
      genesis_time = Chain.genesis_time()
      next_index = Block.next_index(time)
      pool = Pool.fetch_and_check_delay(hostname, time, channel_id)

      if is_nil(pool), do: throw(0)
      if pool.address != Address.to_internal_address(pubkey), do: throw(0)

      data =
        pool_params
        |> CBOR.encode()

      tx =
        %Tx{
          block_index: next_index,
          out_count: 0,
          amount: 0,
          sigs: [signature],
          time: time,
          type: type,
          status: @status_approved,
          vsn: version,
          memo: true,
          inputs: [],
          outputs: []
        }
        |> put_hash(data)
        |> put_index(genesis_time)
        |> put_size(data)

      if Falcon.verify(tx.hash, signature, pubkey) == :error, do: throw(40212)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        returning: false,
        prefix: channel_id
      )
      |> TxData.multi_insert(:txdata, tx.index, data, @mime_cbor, channel_id)
      |> Pool.multi_update(:pool, hostname, pool_params, time, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  # delete pool
  def processing(%{
        "channel" => channel_id,
        "sig" => sig64,
        "hostname" => hostname,
        "pool" => pool_params,
        "pubkey" => pubkey64,
        "time" => time,
        "type" => "pool_delete" = type_name,
        "version" => version
      }) do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      if @version != version, do: throw(40200)

      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)
      unless Channel.check_name?(channel_id), do: throw(40230)

      pubkey = Base.decode64!(pubkey64)
      signature = Base.decode64!(sig64)
      genesis_time = Chain.genesis_time()
      next_index = Block.next_index(time)
      pool = Pool.fetch!(hostname, channel_id)

      if is_nil(pool), do: throw(0)
      if pool.address != Address.to_internal_address(pubkey), do: throw(0)

      data =
        pool_params
        |> CBOR.encode()

      tx =
        %Tx{
          block_index: next_index,
          out_count: 0,
          amount: 0,
          sigs: [signature],
          time: time,
          type: type,
          status: @status_approved,
          vsn: version,
          memo: true,
          inputs: [],
          outputs: []
        }
        |> put_hash(data)
        |> put_index(genesis_time)
        |> put_size(data)

      if Falcon.verify(tx.hash, signature, pubkey) == :error, do: throw(40212)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        returning: false,
        prefix: channel_id
      )
      |> TxData.multi_insert(:txdata, tx.index, data, @mime_cbor, channel_id)
      |> Pool.multi_delete(:pool, hostname, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  # coinbase
  def processing(%{
        "channel" => channel_id,
        "outputs" => outputs,
        "sig" => sig64,
        "pubkey" => pubkey64,
        "time" => time,
        "token" => token_id,
        "type" => type_name,
        "version" => version
      })
      when type_name in [
             "coinbase",
             "jackpot",
             "greate jackpot",
             "payconnect",
             "paystream"
           ] do
    try do
      type = type_index(type_name)
      unless type, do: throw(40201)
      unless Channel.check_name?(channel_id), do: throw(40230)
      unless Token.coin?(token_id), do: throw(40222)

      if @version != version, do: throw(40200)

      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)

      # if check_date_last_coinbase(channel_id, type_name, type, time), do: throw(40237)

      out_count = length(outputs)
      if out_count == 0, do: throw(40204)
      if out_count > @max_outputs, do: throw(40217)

      signature = Base.decode64!(sig64)

      {txo, uTokens, unique_addresses, total} =
        Txo.from_request_coinbase!(outputs, @output_type_coinbase)

      if [token_id] != uTokens, do: throw(40213)
      if length(unique_addresses) != out_count, do: throw(40214)

      next_index = Block.next_index(time)
      genesis_time = Chain.genesis_time()

      tx =
        %Tx{
          amount: total,
          block_index: next_index,
          out_count: out_count,
          outputs: txo,
          sigs: [signature],
          time: time,
          type: type,
          vsn: version,
          status: @status_approved
        }
        |> put_hash()
        |> put_index(genesis_time)
        |> put_outputs_index()
        |> put_size()

      token_pubkey = Base.decode64!(pubkey64)

      token = Token.fetch!(token_id, channel_id)

      if token.owner != Address.to_internal_address(token_pubkey), do: throw(40225)

      if Falcon.verify(tx.hash, signature, token_pubkey) == :error, do: throw(40212)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, tx, prefix: channel_id, returning: false)
      |> Ecto.Multi.insert_all(:txo, Txo, tx.outputs, prefix: channel_id, returning: false)
      |> Balance.multi_upsert_incomes(:incomes, txo, tx.time, channel_id)
      |> Token.multi_update_stats(:token, token_id, total, time, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          {:ok, tx}

        err ->
          IO.inspect(err)
          {:error, 500}
      end
    catch
      err ->
        IO.puts(inspect(err))
        {:error, err}
    end
  end

  def processing(
        %{
          "channel" => channel_id,
          "inputs" => inputs,
          "outputs" => outputs,
          "sigs" => sigs,
          "time" => time,
          "type" => type_name,
          "pool" => pool_hostname,
          "version" => version
        } = params
      )
      when type_name in ["regular", "from request", "non refundable", "gift"] do
    try do
      # check version
      if @version != version, do: throw(40200)
      unless Channel.check_name?(channel_id), do: throw(40230)

      # check tx type
      type = type_index(type_name)
      unless type, do: throw(40201)

      # check timeout
      diff_time = abs(Chain.get_time() - time)
      if diff_time > @timeout, do: throw(40202)

      # check size inputs
      in_count = length(inputs)
      if in_count == 0, do: throw(40203)
      if in_count > @max_inputs, do: throw(40218)

      # check size outputs
      out_count = length(outputs)
      if out_count == 0, do: throw(40204)
      if out_count > @max_outputs, do: throw(40217)
      IO.puts("Tx Here aqui")

      # check index and prev block
      next_index = Block.next_index(time)
      prev_block = Chain.prev_block()
      if prev_block == nil, do: throw(40221)
      if prev_block.index > next_index, do: throw(40205)
      IO.puts("Tx Here aqui 2")

      # get utxo
      input_references = Txi.decode_references(inputs)
      IO.inspect("input_references")
      IO.inspect(input_references)
      utxo = Utxo.get(input_references, channel_id)

      #
      # if check_limit_by_block(input_references, next_index, channel_id), do: throw(40229)

      IO.inspect(utxo)
      IO.puts("in_count: #{in_count} - #{length(utxo)}")
      txo = Txo.from_request(outputs)
      if length(utxo) != in_count, do: throw(40206)
      IO.puts("Tx Here aqui 3")

      {utxo_ids, utxo_tokens, utxo_address, utxo_token_values, utxo_total} = Txo.extract(utxo)
      {_txo_ids, txo_tokens, txo_address, txo_token_values, txo_total} = Txo.extract(txo)
      # IO.puts("#{utxo_total} #{txo_total}")

      IO.inspect("token_values")
      IO.inspect(utxo_token_values)
      IO.inspect(txo_token_values)

      if utxo_total != txo_total, do: throw(40207)
      if utxo_token_values != txo_token_values, do: throw(40207)
      IO.puts("Tx Here aqui 4")
      IO.inspect(utxo_tokens)
      IO.inspect(txo_tokens)
      IO.inspect(@token)
      IO.inspect([@token] not in utxo_tokens)

      if @token not in utxo_tokens, do: throw(40213)

      if utxo_tokens != txo_tokens,
        do: throw(40213)

      if Enum.sort(utxo_address) == Enum.sort(txo_address),
        do: throw(40235)

      pool = Pool.fetch!(pool_hostname, channel_id)
      genesis_time = Chain.genesis_time()

      {outgoings, incomes} = extract_balances(utxo, txo)

      IO.inspect("outgoings")
      IO.inspect(outgoings)
      IO.inspect("incomes")
      IO.inspect(incomes)

      memo = Map.get(params, "memo")
      has_memo = not is_nil(memo) and memo != ""

      if has_memo and String.length(memo) > @max_memo_size, do: throw(40238)

      tx =
        %Tx{
          inputs: Txi.create(utxo),
          outputs: txo,
          block_index: next_index,
          in_count: in_count,
          out_count: out_count,
          total_input: txo_total,
          time: time,
          type: type,
          memo: has_memo,
          vsn: version,
          status: @status_approved
        }
        |> put_hash()
        |> put_signatures(sigs, utxo_address)
        |> put_size()
        |> put_index(genesis_time)
        |> put_outputs_index()
        |> put_inputs_index()
        |> put_fees(utxo_address, pool.address, pool.fee, pool.percent)

      Ecto.Multi.new()
      |> Ecto.Multi.insert(:tx, Map.drop(tx, [:outputs, :inputs]),
        prefix: channel_id,
        returning: false
      )
      |> TxData.multi_insert(:txdata, tx.index, memo, @mime_text, channel_id)
      |> Ecto.Multi.insert_all(:txi, Txi, tx.inputs, prefix: channel_id, returning: false)
      |> Ecto.Multi.insert_all(:txo, Txo, tx.outputs, prefix: channel_id, returning: false)
      |> Balance.multi_upsert_outgoings(:outgoings, outgoings, tx.time, channel_id)
      |> Balance.multi_upsert_incomes(:incomes, incomes, tx.time, channel_id)
      |> Repo.transaction()
      |> case do
        {:ok, _} ->
          # tx_pool =
          #   Map.new()
          #   |> Map.take([:block_index, :time, :type])
          #   |> Map.put(:incomes, incomes)
          #   |> Map.put(:outgoings, outgoings)

          # TxPool.put(tx.index, tx_pool)

          # set available txos
          Txo.update_utxo_avail(utxo_ids, channel_id, false)
          Txo.update_txo_avail(tx.index, channel_id, true)

          {:ok, tx}

        err ->
          Logger.error("Tx error database")
          IO.inspect(err)
          {:error, 500}
      end

      # rescue
      # [ArgumentError, MatchError, FunctionClauseError, ErlangError] ->
      #   Logger.error("Tx error catch")
      #   {:error, 400}
    catch
      err ->
        Logger.error("Tx error code #{err}")
        {:error, err}
    end
  end

  def processing(_), do: {:error, 40000}

  # def after_process(txid, time, type, channel_id) do
  #   outputs_outgoings =
  #   from(txo in Txo,
  #   join: tx in Tx,
  #   on: fragment("? = substring(?::bytea from 1 for length(?))", txid, txo.id, txid),
  #   where: tx.id == ^txid,
  #   select: count()
  # )

  # outputs_incomes =
  #   from(txo in Txo,
  #     join: tx in Tx,
  #     on: fragment("? = substring(?::bytea from 1 for length(?))", txid, txo.id, txid),
  #     where: tx.id == ^txid,
  #     select: count()
  #   )

  #   from(txo in Txo, where: )

  #   Ecto.Multi.new()
  #   |> Balance.multi_update_outgoings(outputs_outgoings, time, channel_id)
  #   |> Balance.multi_upsert_incomes(:incomes, outputs_incomes, tx.time, channel_id)
  #   |> Repo.transaction()

  # end

  def exists?(index, channel_id) do
    from(tx in Tx, where: tx.index == ^index)
    |> Repo.exists?(prefix: channel_id)
  end

  def check_limit_by_block(input_addresses, current_block, channel_id) do
    from(txo in Txo,
      join: tx in Tx,
      on: fragment("? = substring(?::bytea from 1 for length(?))", tx.index, txo.id, tx.index),
      where: txo.address in ^input_addresses and tx.block_index == ^current_block,
      select: count()
    )
    |> Repo.one(prefix: channel_id) > 0
  end

  # compute tuple {outgoings, incomes}
  defp extract_balances(utxos, txos) do
    balances =
      (Enum.map(utxos, &Map.put(&1, :value, -&1.value)) ++ txos)
      |> Enum.group_by(&{&1.address, &1.tid}, & &1.value)
      |> Map.to_list()
      |> Enum.map(fn {{address, token}, values} ->
        %{address: address, tid: token, value: Enum.sum(values)}
      end)

    balances
    |> Enum.reduce({[], []}, fn x, {oacc, iacc} ->
      cond do
        x.value < 0 ->
          {oacc ++ [x], iacc}

        true ->
          {oacc, iacc ++ [x]}
      end
    end)
  end

  defp extract_sigs_b64!(hash, sigs, utxo_address) do
    {signatures, addresses} =
      Enum.reduce(sigs, {[], []}, fn %{
                                       "address" => arr_address58,
                                       "pubkey" => pubkey64,
                                       "sig" => sig64
                                     } = params,
                                     {acc_sig, acc_addr} ->
        pubkey = Base.decode64!(pubkey64)

        IO.inspect("arr_address58")
        IO.inspect(arr_address58)

        arr_address =
          Enum.reduce(arr_address58, [], fn address58, acc_addr ->
            address = Base58Check.decode(address58)

            access_method =
              case params do
                %{"exkey" => exkey, "data" => data} ->
                  {pubkey, Base.decode64!(exkey), Base.decode64!(data, ignore: :whitespace)}

                _ ->
                  pubkey
              end

            case Address.check_address_pubkey(address, access_method) do
              true ->
                acc_addr ++ [address]

              false ->
                throw(40210)
            end
          end)

        sig = Base.decode64!(sig64, ignore: :whitespace)

        case Falcon.verify(hash, sig, pubkey) do
          :ok ->
            {acc_sig ++ [sig], acc_addr ++ arr_address}

          _ ->
            throw(40212)
        end
      end)

    if addresses != Enum.uniq(utxo_address), do: throw(40211)

    signatures
  end

  # coinbase compute size
  def calc_size(type, tx) when type in 0..99 do
    pre_index_size = byte_size(:binary.encode_unsigned(tx.block_index))
    output_index_size = pre_index_size + 8 + 3
    sig_size = length(tx.sigs) * 625

    @base_size + sig_size + Txo.calc_size(tx.outputs, output_index_size)
  end

  def calc_size(_type, tx) do
    block_index_size = byte_size(:binary.encode_unsigned(tx.block_index))
    tx_index_size = block_index_size + 8
    output_index_size = block_index_size + 8 + 3
    sig_size = length(tx.sigs) * 625
    output_size = Txo.calc_size(tx.outputs, output_index_size)
    input_size = Txi.calc_size(tx.inputs, tx_index_size)

    IO.inspect("block_index_size: #{block_index_size}")
    IO.inspect("tx_index_size: #{tx_index_size}")
    IO.inspect("output_index_size: #{output_index_size}")
    IO.inspect("output_size: #{output_size}")
    IO.inspect("input_size: #{input_size}")
    IO.inspect("sig_size: #{sig_size}")

    @base_size + sig_size + output_size + input_size
  end

  # compute Tx size with tx data
  def calc_size(type, tx, tx_data) when type in 1000..1999 do
    sig_size = length(tx.sigs) * 625
    @base_size + sig_size + byte_size(tx_data)
  end

  @spec calc_fees(integer, float, boolean, integer) :: integer
  def calc_fees(amount, pool_fee, true, _size),
    do: :math.ceil(amount * (pool_fee / 100)) |> trunc()

  def calc_fees(_amount, pool_fee, _false, _size), do: pool_fee |> trunc()

  @spec extract_amounts!(List.t(), List.t(), binary) ::
          {pos_integer(), pos_integer(), pos_integer()}
  defp extract_amounts!(outputs, utxo_addresses, pool_address) do
    IO.inspect("utxo_addresses")
    IO.inspect(utxo_addresses)

    r =
      {tx_amount, fees_amount, returned_amount} =
      Enum.reduce(outputs, {0, 0, 0}, fn x, {acc_amount, acc_fees, acc_returned} ->
        cond do
          x.tid == @token and
            x.address == pool_address and
              x.type == @output_type_fee ->
            IO.inspect("pool_address amount: #{x.value}")
            {acc_amount, acc_fees + x.value, acc_returned}

          x.address in utxo_addresses ->
            if x.type != @output_type_return, do: throw(40220)

            {acc_amount, acc_fees, acc_returned + x.value}

          true ->
            {acc_amount + x.value, acc_fees, acc_returned}
        end
      end)

    IO.inspect(r)

    # if pool_amount != ceil(fees_amount * (pool_amount / 100)), do: throw(40220)

    {tx_amount, fees_amount, returned_amount}
  end

  defp check_date_last_coinbase(channel, type_name, type, timestamp)
       when type_name in ["paystream", "payconnect"] do
    from(tx in Tx,
      where: tx.status == @status_complete and tx.type == ^type,
      select: tx.time,
      order_by: [desc: fragment("length(?)", tx.index), desc: tx.index]
    )
    |> Repo.one(prefix: channel)
    |> case do
      nil ->
        false

      unix_time ->
        dateLastTx = DateTime.from_unix!(unix_time, :millisecond)
        date = DateTime.from_unix!(timestamp, :millisecond)

        dateLastTx < date and dateLastTx.day < date.day
    end
  end

  defp check_date_last_coinbase(channel, type_name, type, timestamp)
       when type_name == "greate jackpot" do
    from(tx in Tx,
      where: tx.status == @status_complete and tx.type == ^type,
      select: tx.time,
      order_by: [desc: fragment("length(?)", tx.index), desc: tx.index]
    )
    |> Repo.one(prefix: channel)
    |> case do
      nil ->
        false

      unix_time ->
        dateLastTx = DateTime.from_unix!(unix_time, :millisecond)
        date = DateTime.from_unix!(timestamp, :millisecond)

        dateLastTx < date and dateLastTx.month < date.month
    end
  end

  defp check_date_last_coinbase(_channel, _type_name, _type, _time), do: false

  def get_all_pending(channel) do
    from(tx in Tx,
      where: tx.status == @status_pending,
      select: %{index: tx.index, hash: tx.hash, amount: tx.amount, type: tx.type},
      order_by: [asc: fragment("length(?)", tx.index), asc: tx.index]
    )
    |> Repo.all(prefix: channel)
  end

  def get_all_approved(channel) do
    from(tx in Tx,
      where: tx.status == @status_approved,
      select: %{
        index: tx.index,
        hash: tx.hash,
        amount: tx.amount,
        type: tx.type,
        block_index: tx.block_index
      },
      order_by: [asc: fragment("length(?)", tx.index), asc: tx.index]
    )
    |> Repo.all(prefix: channel)
  end

  def get_all_approved(next_index, channel) do
    from(tx in Tx,
      where: tx.status == @status_approved and tx.block_index == ^next_index,
      select: %{index: tx.index, hash: tx.hash, amount: tx.amount, type: tx.type},
      order_by: [asc: fragment("length(?)", tx.index), asc: tx.index]
    )
    |> Repo.all(prefix: channel)
  end

  # def get_from_block(block_index, channel) do
  #   from(tx in Tx,
  #     where: tx.status == @status_approved and tx.block_index == ^block_index,
  #     select: %{index: tx.index, hash: tx.hash, amount: tx.amount, type: tx.type},
  #     order_by: [asc: fragment("length(?)", tx.index), asc: tx.index]
  #   )
  #   |> Repo.all(prefix: channel)
  # end

  # [tx_first | txs_rest],
  def set_status_complete(multi, name, block_index, channel) do
    query =
      from(tx in Tx, where: tx.status == @status_approved and tx.block_index == ^block_index)

    Ecto.Multi.update_all(multi, name, query, [set: [status: @status_complete]],
      prefix: channel,
      returning: false
    )
  end

  def cancel_all_pending(multi, name, block_index, channel) do
    query = from(tx in Tx, where: tx.status == @status_pending and tx.block_index <= ^block_index)

    Ecto.Multi.update_all(multi, name, query, [set: [status: @status_timeout]],
      prefix: channel,
      returning: false
    )
  end

  def all?(txs_ids, channel) do
    res =
      from(tx in Tx,
        where: tx.status == @status_approved and tx.index in ^txs_ids,
        select: count()
      )
      |> Repo.all(prefix: channel)

    length(res) == length(txs_ids)
  end

  def get(hash, params) do
    # from(tx in Tx, where: tx.status == @status_complete and tx.hash == ^hash)
    from(tx in Tx,
      left_join: txd in TxData,
      on: tx.memo and tx.index == txd.txid,
      where: tx.hash == ^hash,
      select: map_select()
    )
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform_one()
  end

  def get_by_index(txid, params) do
    from(tx in Tx,
      left_join: txd in TxData,
      on: tx.memo and tx.index == txd.txid,
      where: tx.index == ^txid,
      select: map_select()
    )
    |> Repo.one(prefix: filter_channel(params, Default.channel()))
    |> transform_one()
  end

  @spec fetch_inputs(binary, binary) :: [Txo.t()]
  def fetch_inputs(txid, channel_id) do
    from(txi in Txi,
      join: txo in Txo,
      on: txi.oid == txo.id,
      where: txi.txid == ^txid,
      order_by: [asc: fragment("length(?)", txo.oid), asc: txo.oid]
    )
    |> Repo.all(prefix: channel_id)
  end

  @spec fetch_outputs(binary, binary) :: [Txo.t()]
  def fetch_outputs(txid, channel_id) do
    from(txo in Txo,
      where: fragment("substring(?::bytea from 1 for ?) = ?", txo.id, ^byte_size(txid), ^txid),
      order_by: [asc: fragment("length(?)", txo.oid), asc: txo.oid]
    )
    |> Repo.all(prefix: channel_id)
  end

  @spec get_refundable(binary, integer, binary) :: nil | Tx.t()
  def get_refundable(txid, timestamp, channel_id) do
    from(tx in Tx,
      where:
        tx.index == ^txid and
          tx.status == @status_complete and
          tx.type in [100, 101, 103] and
          ^timestamp < tx.time + @timeout_refund
    )
    |> Repo.one(prefix: channel_id)
  end

  def all(params) do
    from(Tx)
    |> where([tx], tx.status == @status_complete)
    |> join(:left, [tx], txd in TxData, on: tx.memo and tx.index == txd.txid)
    |> filter_index(params)
    |> filter_select(params)
    |> filter_offset(params)
    |> filter_status(params)
    |> filter_date(params)
    |> filter_limit(params, 50, 100)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform()
  end

  defp filter_index(query, %{"hash" => hash}) do
    where(query, [tx], tx.hash == ^hash)
  end

  defp filter_index(query, %{"block_index" => block_index}) do
    where(query, [tx], tx.block_index == ^block_index)
  end

  defp filter_index(query, %{"q" => q}) do
    binq = Utils.decode16(q)

    # ilike(tx.hash, ^"#{binq}%")
    where(query, [tx], tx.hash == ^binq)
  end

  defp filter_index(query, _), do: query

  defp filter_status(query, %{"status" => status}) do
    where(query, [tx], tx.status == ^status)
  end

  defp filter_status(query, _), do: query

  def filter_date(query, %{"from" => from_date, "to" => to_date}) do
    date_start = Utils.from_date_to_time(from_date, :start, @unit_time)

    date_end = Utils.from_date_to_time(to_date, :end, @unit_time)

    where(query, [tx], tx.time >= ^date_start and tx.time <= ^date_end)
  end

  def filter_date(query, %{"from" => from_date}) do
    date_start = Utils.from_date_to_time(from_date, :start, @unit_time)

    where(query, [tx], tx.time >= ^date_start)
  end

  def filter_date(query, %{"to" => to_date}) do
    date_end = Utils.from_date_to_time(to_date, :end, @unit_time)

    where(query, [tx], tx.time <= ^date_end)
  end

  def filter_date(query, _), do: query

  defp filter_select(query, _), do: select(query, [tx, txd], map_select())

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [tx], asc: fragment("length(?)", tx.index), asc: tx.index)

      "most_value" ->
        order_by(query, [tx], desc: tx.amount)

      "less_value" ->
        order_by(query, [tx], asc: tx.amount)

      _ ->
        order_by(query, [tx], desc: fragment("length(?)", tx.index), desc: tx.index)
    end
  end

  defp transform(nil), do: []
  defp transform([]), do: []

  defp transform(data) do
    Enum.map(data, fn x ->
      transform_one(x)
    end)
  end

  defp transform_one(x) do
    %{
      index: encode_index(x.index),
      status: status_name(x.status),
      type: type_name(x.type),
      memo: TxData.decode!(x.memo, x.mime),
      amount: x.amount,
      block_index: x.block_index,
      fees: x.fees,
      hash: Base.encode16(x.hash),
      in_count: x.in_count,
      out_count: x.out_count,
      size: x.size,
      sig_count: x.sig_count,
      time: x.time,
      total_input: x.total_input,
      total_output: x.total_output,
      vsn: x.vsn
    }
  end

  def from_struct(tx) do
    Map.drop(tx, [:__meta__, :inputs, :outputs, :block])
    |> Map.from_struct()
  end
end
