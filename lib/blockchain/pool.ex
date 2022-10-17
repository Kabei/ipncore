defmodule Ipncore.Pool do
  use Ecto.Schema
  import Ecto.Query
  import Ipnutils.Filters
  alias Ipncore.Repo
  alias __MODULE__

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @edit_fields ~w(address hostname name fee percent)

  @primary_key {:hostname, :string, []}
  schema "pools" do
    field(:name, :string)
    field(:address, :string)
    field(:fee, :float)
    field(:percent, :boolean)
    field(:enabled, :boolean, default: true)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  def new(
        %{
          "name" => name,
          "address" => address,
          "fee" => fee,
          "percent" => percent
        },
        time
      ) do
    %Pools{
      id: token_id,
      name: name,
      address: address,
      fee: fee,
      percent: percent,
      created_at: time,
      updated_at: time
    }
  end

  def new(_), do: throw(0)

  def get(hostname, channel) do
    from(p in Pools, where: p.hostname == ^hostname and p.enabled)
    |> Repo.one(prefix: channel)
  end

  def get(hostname, channel) do
    from(p in Pools, where: p.address == ^address and p.enabled)
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
    query = from(p in Pool, where: p.hostname == ^hostname and p.updated_at + @delay < time)

    edit_params =
      params
      |> Map.take(@edit_fields)
      |> Enum.map(fn {k, v} -> {String.to_atom(k), v} end)
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
    from(__MODULE__)
    |> filter_hostname(params)
    |> filter_limit(params)
    |> filter_offset(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
  end

  defp filter_hostname(query, %{"q" => q}) do
    q = "%#{q}%"
    where(query, [pool], ilike(pool.hostname, ^q) or ilike(pool.name, ^q))
  end

  defp filter_hostname(query, _), do: query
end
