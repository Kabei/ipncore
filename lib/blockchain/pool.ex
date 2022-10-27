defmodule Ipncore.Pool do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.{Block, Chain, Repo, Tx}
  alias __MODULE__

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(address hostname name fee percent)
  @edit_fields ~w(address hostname name fee percent)

  @primary_key {:hostname, :string, []}
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
          "hostname" => hostname,
          "name" => name,
          "address" => address,
          "fee" => fee,
          "percent" => percent
        },
        time
      )
      when fee > 0 do
    %Pool{
      hostname: hostname,
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

  def fetch!(hostname, channel) do
    from(p in Pool, where: p.hostname == ^hostname and p.enabled)
    |> Repo.one!(prefix: channel)
  end

  def fetch_and_check_delay(hostname, time, channel) do
    from(p in Pool,
      where: p.hostname == ^hostname and p.enabled and p.updated_at + @delay_edit < ^time
    )
    |> Repo.one(prefix: channel)
  end

  def get(hostname, channel) do
    from(p in Pool,
      where: p.hostname == ^hostname and p.enabled,
      select: %{
        hostname: p.hostname,
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

  def exists?(hostname, channel) do
    from(p in Pool, where: p.hostname == ^hostname and p.enabled)
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

  def multi_update(multi, name, hostname, params, time, channel) do
    query = from(p in Pool, where: p.hostname == ^hostname)

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

  def multi_delete(multi, name, hostname, channel) do
    query = from(p in Pool, where: p.hostname == ^hostname)

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
    |> filter_hostname(params)
    |> filter_select()
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> filter_map()
  end

  defp filter_hostname(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [pool], ilike(pool.hostname, ^q) or ilike(pool.name, ^q))
  end

  defp filter_hostname(query, _), do: query

  defp filter_select(query) do
    select(query, [p], %{
      hostname: p.hostname,
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
            "hostname" => _hostname,
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
        "hostname" => hostname,
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
    pool = fetch_and_check_delay(hostname, time, channel)

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
    |> multi_update(:pool, hostname, pool_params, time, channel)
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
        "id" => hostname,
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
    pool = Pool.fetch!(hostname, channel_id)
    if is_nil(pool), do: throw(0)
    if pool.address != Address.to_internal_address(pubkey), do: throw(0)

    data =
      %{"id" => hostname}
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
    |> multi_delete(:pool, hostname, channel)
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
