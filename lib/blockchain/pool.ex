defmodule Ipncore.Validator do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Block, Chain, Repo, Tx}
  alias __MODULE__

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(address host name fee percent)
  @edit_fields ~w(address host name fee percent)

  @db :validator
  @db_opts [auto_save: 60_000]

  @primary_key {:host, :string, []}
  schema "pools" do
    field(:name, :string)
    field(:address, :binary)
    field(:fee, :float)
    field(:percent, :boolean)
    field(:enabled, :boolean, default: true)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  def new(
        %{
          "host" => host,
          "name" => name,
          "address" => address,
          "fee" => fee,
          "percent" => percent
        },
        time
      )
      when fee > 0 do
    %Pool{
      host: host,
      name: name,
      address: address,
      fee: fee,
      percent: percent,
      created_at: time,
      updated_at: time
    }
  end

  def new(_), do: throw(0)

  def filter_data(params), do: Map.take(params, @fields)

  def fetch!(host, channel) do
    from(p in Pool, where: p.host == ^host and p.enabled)
    |> Repo.one!(prefix: channel)
  end

  def fetch_and_check_delay(host, time, channel) do
    from(p in Pool,
      where: p.host == ^host and p.enabled and p.updated_at + @delay_edit < ^time
    )
    |> Repo.one(prefix: channel)
  end

  def get(host, channel) do
    from(p in Pool,
      where: p.host == ^host and p.enabled,
      select: %{
        host: p.host,
        name: p.name,
        address: p.address,
        fee: p.fee,
        percent: p.percent,
        created_at: p.created_at,
        updated_at: p.updated_at
      }
    )
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def exists?(host, channel) do
    from(p in Pool, where: p.host == ^host and p.enabled)
    |> Repo.exists?(prefix: channel)
  end

  def multi_insert(multi, name, pool, time, channel) do
    pool_struct = new(pool, time)

    Ecto.Multi.insert(
      multi,
      name,
      pool_struct,
      returning: false,
      prefix: channel
    )
  end

  def multi_update(multi, name, host, params, time, channel) do
    query = from(p in Pool, where: p.host == ^host)

    params =
      params
      |> Map.take(@edit_fields)
      |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
      |> Keyword.put(:updated_at, time)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: params],
      returning: false,
      prefix: channel
    )
  end

  def multi_delete(multi, name, host, channel) do
    query = from(p in Pool, where: p.host == ^host)

    Ecto.Multi.delete_all(
      multi,
      name,
      query,
      returning: false,
      prefix: channel
    )
  end

  def all(params) do
    from(p in Pool, where: p.enabled)
    |> filter_host(params)
    |> filter_select()
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_host(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [pool], ilike(pool.host, ^q) or ilike(pool.name, ^q))
  end

  defp filter_host(query, _), do: query

  defp filter_select(query) do
    select(query, [p], %{
      host: p.host,
      name: p.name,
      address: p.address,
      fee: p.fee,
      percent: p.percent,
      created_at: p.created_at,
      updated_at: p.updated_at
    })
  end

  defp filter_map(data) do
    Enum.map(data, fn x -> transform(x) end)
  end

  defp transform(x), do: %{x | address: Base58Check.encode(x.address)}

  # pool new
  def processing(%{
        "channel" => channel,
        "time" => time,
        "data" =>
          %{
            "host" => _host,
            "address" => address58,
            "fee" => _fee,
            "percent" => _percent
          } = pool,
        "type" => 1300 = type,
        "sig" => sig64,
        "version" => version
      }) do
    address = Base58Check.decode(address58)
    next_index = Block.next_index(time)
    genesis_time = Chain.genesis_time()
    signature = Base.decode64!(sig64)

    pool = Map.put(pool, "address", address)

    data =
      pool
      |> filter_data()
      |> CBOR.encode()

    tx =
      %{
        block_index: next_index,
        sigs: [signature],
        time: time,
        type: type,
        status: Tx.status_approved(),
        vsn: version
      }
      |> Tx.put_hash(data)
      |> Tx.put_index(genesis_time)
      |> Tx.put_size(data)
      |> Tx.verify_sign(signature, PlatformOwner.pubkey())

    Ecto.Multi.new()
    |> Tx.multi_insert(:tx, tx, channel)
    |> multi_insert(:pool, pool, time, channel)
    |> Repo.transaction()
    |> case do
      {:ok, _} ->
        {:ok, tx}

      err ->
        IO.inspect(err)
        {:error, 500}
    end
  end

  # update pool
  def processing(%{
        "channel" => channel,
        "sig" => sig64,
        "host" => host,
        "data" => pool_params,
        "pubkey" => pubkey64,
        "time" => time,
        "type" => 1301 = type,
        "version" => version
      }) do
    pubkey = Base.decode64!(pubkey64)
    signature = Base.decode64!(sig64)
    genesis_time = Chain.genesis_time()
    next_index = Block.next_index(time)
    pool = fetch_and_check_delay(host, time, channel)

    if is_nil(pool), do: throw(0)
    if pool.address != Address.to_internal_address(pubkey), do: throw(0)

    data =
      pool_params
      |> CBOR.encode()

    tx =
      %{
        block_index: next_index,
        sigs: [signature],
        time: time,
        type: type,
        status: Tx.status_approved(),
        vsn: version
      }
      |> Tx.put_hash(data)
      |> Tx.put_index(genesis_time)
      |> Tx.put_size(data)
      |> Tx.verify_sign(signature, pubkey)

    Ecto.Multi.new()
    |> Tx.multi_insert(tx, channel)
    |> multi_update(:pool, host, pool_params, time, channel)
    |> Repo.transaction()
    |> case do
      {:ok, _} ->
        {:ok, tx}

      err ->
        IO.inspect(err)
        {:error, 500}
    end
  end

  def processing(%{
        "channel" => channel,
        "sig" => sig64,
        "id" => host,
        "pubkey" => pubkey64,
        "time" => time,
        "type" => 1302 = type,
        "version" => version
      }) do
    pubkey = Base.decode64!(pubkey64)
    signature = Base.decode64!(sig64)
    genesis_time = Chain.genesis_time()
    next_index = Block.next_index(time)

    # fetch data and check owner
    pool = Pool.fetch!(host, channel_id)
    if is_nil(pool), do: throw(0)
    if pool.address != Address.to_internal_address(pubkey), do: throw(0)

    data =
      %{"id" => host}
      |> CBOR.encode()

    tx =
      %{
        block_index: next_index,
        sigs: [signature],
        time: time,
        type: type,
        status: Tx.status_approved(),
        vsn: version
      }
      |> Tx.put_hash(data)
      |> Tx.put_index(genesis_time)
      |> Tx.put_size(data)
      |> Tx.verify_sign(signature, pubkey)

    Ecto.Multi.new()
    |> Tx.multi_insert(tx, channel)
    |> multi_delete(:pool, host, channel)
    |> Repo.transaction()
    |> case do
      {:ok, _} ->
        {:ok, tx}

      err ->
        IO.inspect(err)
        {:error, 500}
    end
  end
end
