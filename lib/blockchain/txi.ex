defmodule Ipncore.Txi do
  use Ecto.Schema
  alias Ipncore.{Address, Token, Event, Repo}

  import Ecto.Query
  import Ipnutils.Filters
  alias __MODULE__

  @primary_key false
  schema "txi" do
    field(:txid, :binary)
    field(:ix, :integer)
    field(:address, :binary)
    field(:token, :string)
    field(:value, :integer)
  end

  def multi_insert_all(multi, _name, nil, _channel), do: multi
  def multi_insert_all(multi, _name, [], _channel), do: multi

  def multi_insert_all(multi, name, inputs, channel) do
    Ecto.Multi.insert_all(multi, name, Txi, inputs, prefix: channel, returning: false)
  end

  def all(params) do
    from(Txi)
    |> filter_index(params)
    |> filter_address(params)
    |> filter_token(params)
    |> filter_offset(params)
    |> filter_limit(params, 50, 100)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform(params)
  end

  defp filter_index(query, %{"hash" => hash16}) do
    hash = Base.decode16!(hash16, case: :mixed)

    sub = from(ev in Event, where: ev.hash == ^hash, select: ev.id)

    where(query, [txi], txi.txid == subquery(sub))
  end

  defp filter_index(query, %{"txid" => txid}) do
    where(query, [txi], txi.txid == ^Event.decode_id(txid))
  end

  defp filter_index(query, _params), do: query

  defp filter_address(query, %{"address" => address}) do
    bin_address = Address.from_text(address)
    where(query, [txi], txi.address == ^bin_address)
  end

  defp filter_address(query, _params), do: query

  defp filter_token(query, %{"token" => token}) do
    where(query, [txi], txi.token == ^token)
  end

  defp filter_token(query, _params), do: query

  defp filter_select(query, %{"show" => "props"}) do
    query
    |> join(:inner, [txi], tk in Token, on: tk.id == txi.token)
    |> select([txi, tk], %{
      txid: txi.txid,
      ix: txi.ix,
      address: txi.address,
      token: txi.token,
      value: txi.value,
      decimals: tk.decimals,
      symbol: tk.symbol
    })
  end

  defp filter_select(query, %{"fmt" => "list"}) do
    select(query, [txi], [
      txi.txid,
      txi.ix,
      txi.address,
      txi.token,
      txi.value
    ])
  end

  defp filter_select(query, _params) do
    select(query, [txi], %{
      txid: txi.oid,
      ix: txi.ix,
      address: txi.address,
      token: txi.token,
      value: txi.value
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "newest" ->
        order_by(query, [txi], desc: fragment("length(?)", txi.txid), desc: txi.txid, asc: txi.ix)

      _ ->
        order_by(query, [txi], asc: fragment("length(?)", txi.txid), asc: txi.txid, asc: txi.ix)
    end
  end

  defp transform(txis, %{"fmt" => "list"}) do
    Enum.map(txis, fn [id, ix, address, token, value] ->
      [Event.encode_id(id), ix, Address.to_text(address), token, value]
    end)
  end

  defp transform(txis, _) do
    Enum.map(txis, fn x ->
      %{x | id: Event.encode_id(x.id), address: Address.to_text(x.address)}
    end)
  end
end
