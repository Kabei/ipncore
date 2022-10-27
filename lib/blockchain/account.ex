defmodule Ipncore.Account do
  alias __MODULE__
  use Ecto.Schema
  import Ecto.Query
  alias Ipncore.Repo

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(username address email avatar phone props)
  @edit_fields ~w(username enabled address email avatar phone props)

  @primary_key {:did, :string, []}
  schema "account" do
    field(:address, :binary)
    field(:name, :string)
    field(:avatar, :string)
    field(:email, :string)
    field(:phone, :string)
    field(:enabled, :boolean, default: true)
    field(:props, :map)
    field(:created_at, :integer)
    field(:updated_at, :integer)
  end

  def cast(params, filter, time) do
    Map.take(params, filter)
    |> Enum.map(fn {k, v} -> {String.to_existing_atom(k), v} end)
    |> Keyword.merge(%{created_at: time, updated_at: time})
  end

  def new(%{"username" => username, "address" => address} = params, time) do
    data = cast(params, @fields)

    struct(Account, data)
  end

  def new(_), do: throw(0)

  def fetch!(username, channel) do
    from(a in Account, where: a.username == ^username and a.enabled)
    |> Repo.one!(prefix: channel)
  end

  def fetch_and_check_delay(username, time, channel) do
    from(a in Account,
      where: a.username == ^username and a.enabled and a.updated_at + @delay_edit < ^time
    )
    |> Repo.one(prefix: channel)
  end

  def get(username, channel) do
    from(a in Account, where: a.username == ^username and a.enabled)
    |> Repo.one(prefix: channel)
    |> transform()
  end

  def exists?(username, channel) do
    from(a in Account, where: a.enabled and a.username == ^username)
    |> Repo.exists?(prefix: channel)
  end

  def multi_insert(multi, name, data, time, channel) do
    data_struct = new(data, time)

    Ecto.Multi.insert(
      multi,
      name,
      data_struct,
      returning: false,
      prefix: channel
    )
  end

  def multi_update(multi, name, username, params, time, channel, opts \\ []) do
    query =
      case Keyword.get(opts, :check_delay, false) do
        true ->
          from(a in Account, where: a.username == ^username)

        false ->
          from(a in Account,
            where: a.username == ^username and a.enabled and a.updated_at + @delay_edit < ^time
          )
      end

    params = cast(params, @edit_fields, time)

    Ecto.Multi.update_all(
      multi,
      name,
      query,
      [set: params],
      returning: false,
      prefix: channel
    )
  end

  def multi_delete(multi, name, username, channel) do
    query = from(a in Account, where: a.username == ^username)

    Ecto.Multi.delete_all(
      multi,
      name,
      query,
      returning: false,
      prefix: channel
    )
  end
end
