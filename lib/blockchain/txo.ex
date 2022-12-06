defmodule Ipncore.Txo do
  use Ecto.Schema
  alias Ipncore.{Address, Event, Token, Repo}
  import Ecto.Query, only: [from: 1, from: 2, where: 3, order_by: 3, select: 3, join: 5]
  import Ipnutils.Filters
  alias __MODULE__

  @primary_key false
  schema "txo" do
    field(:txid, :binary)
    field(:ix, :integer)
    field(:token, :string)
    field(:from, :binary)
    field(:to, :binary)
    field(:value, :integer)
    field(:reason, :string)
  end

  def multi_insert_all(multi, _name, nil, _channel), do: multi
  def multi_insert_all(multi, _name, [], _channel), do: multi

  def multi_insert_all(multi, name, outputs, channel) do
    Ecto.Multi.insert_all(multi, name, Txo, outputs, prefix: channel, returning: false)
  end

  def all(params) do
    from(Txo)
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
    where(query, [txo], txo.txid == subquery(sub))
  end

  defp filter_index(query, %{"txid" => txid}) do
    txid = Event.decode_id(txid)

    where(query, [txo], txo.txid == ^txid)
  end

  defp filter_index(query, _params), do: query

  defp filter_address(query, %{"from" => address}) do
    bin_address = Address.from_text(address)
    where(query, [txo], txo.from == ^bin_address)
  end

  defp filter_address(query, %{"to" => address}) do
    bin_address = Address.from_text(address)
    where(query, [txo], txo.to == ^bin_address)
  end

  defp filter_address(query, %{"address" => address}) do
    bin_address = Address.from_text(address)
    where(query, [txo], txo.to == ^bin_address or txo.address == ^bin_address)
  end

  defp filter_address(query, _params), do: query

  defp filter_token(query, %{"token" => token}) do
    where(query, [txo], txo.token == ^token)
  end

  defp filter_token(query, _params), do: query

  defp filter_select(query, %{"show" => "props"}) do
    query
    |> join(:inner, [o], tk in Token, on: tk.id == o.token)
    |> select([o, tk], %{
      txid: o.txid,
      ix: o.ix,
      from: o.from,
      to: o.to,
      token: o.token,
      reason: o.reason,
      value: o.value,
      decimals: tk.decimals,
      symbol: tk.symbol
    })
  end

  defp filter_select(query, %{"fmt" => "list"}) do
    select(query, [o], [
      o.txid,
      o.ix,
      o.from,
      o.to,
      o.token,
      o.reason,
      o.value
    ])
  end

  defp filter_select(query, _params) do
    select(query, [o], %{
      txid: o.txid,
      ix: o.ix,
      token: o.token,
      from: o.from,
      to: o.to,
      reason: o.reason,
      value: o.value
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "newest" ->
        order_by(query, [txo], desc: fragment("length(?)", txo.txid), desc: txo.txid, asc: txo.ix)

      _ ->
        order_by(query, [txo], asc: fragment("length(?)", txo.txid), asc: txo.txid, asc: txo.ix)
    end
  end

  defp transform(txos, %{"fmt" => "list"}) do
    Enum.map(txos, fn [id, ix, token, from, to, reason, value] ->
      [
        Event.encode_id(id),
        ix,
        token,
        Address.to_text(from),
        Address.to_text(to),
        reason,
        value
      ]
    end)
  end

  defp transform(txos, _) do
    Enum.map(txos, fn x ->
      %{x | txid: Event.encode_id(x.txid), from: Address.to_text(x.from), to: Address.to_text(x.to)}
    end)
  end
end
