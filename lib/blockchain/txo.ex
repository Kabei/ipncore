defmodule Ipncore.Txo do
  use Ecto.Schema
  alias Ippan.{Address, Utils}
  alias Ipncore.{Event, Repo, Token}
  import Ecto.Query, only: [from: 2, where: 3, order_by: 3, select: 3, join: 5]
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
    from(txo in Txo,
      join: ev in Event,
      on: ev.hash == txo.txid
    )
    |> filter_index(params)
    |> filter_address(params)
    |> filter_token(params)
    |> filter_reason(params)
    |> filter_offset(params)
    |> filter_limit(params)
    |> filter_select(params)
    |> sort(params)
    |> Repo.all(prefix: filter_channel(params, Default.channel()))
    |> transform(params)
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

  defp filter_address(query, %{"from" => from_address, "to" => to_address}) do
    to_address = Address.from_text(to_address)
    from_address = Address.from_text(from_address)
    where(query, [txo], txo.from == ^from_address and txo.to == ^to_address)
  end

  defp filter_address(query, %{"activity" => address}) do
    address = Address.from_text(address)
    where(query, [txo], txo.from == ^address or txo.to == ^address)
  end

  defp filter_address(query, _params), do: query

  defp filter_token(query, %{"token" => token}) do
    where(query, [txo], txo.token == ^token)
  end

  defp filter_token(query, _params), do: query

  defp filter_reason(query, %{"reason" => reason}) do
    where(query, [txo], txo.reason == ^reason)
  end

  defp filter_reason(query, _), do: query

  # defp filter_select(query, %{"fmt" => "list"}) do
  #   select(query, [o, ev], [
  #     o.txid,
  #     o.from,
  #     o.to,
  #     o.token,
  #     o.reason,
  #     o.value,
  #     ev.time
  #   ])
  # end

  defp filter_select(query, _params) do
    query
    |> join(:inner, [o], tk in Token, on: tk.id == o.token)
    |> select([o, ev, tk], %{
      id: o.txid,
      from: o.from,
      to: o.to,
      token: o.token,
      time: ev.time,
      reason: o.reason,
      value: o.value,
      decimals: tk.decimals
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "oldest" ->
        order_by(query, [txo, ev], asc: ev.time, desc: txo.txid, asc: txo.ix)

      "most_value" ->
        order_by(query, [txo], desc: txo.value)

      "less_value" ->
        order_by(query, [txo], asc: txo.value)

      _ ->
        order_by(query, [txo, ev], desc: ev.time, asc: txo.txid, asc: txo.ix)
    end
  end

  # defp transform(txos, %{"fmt" => "list"}) do
  #   Enum.map(txos, fn [id, token, from, to, reason, value] ->
  #     [
  #       Event.encode_id(id),
  #       token,
  #       Address.to_text(from),
  #       Address.to_text(to),
  #       reason,
  #       value
  #     ]
  #   end)
  # end

  defp transform(txos, _) do
    Enum.map(txos, fn x ->
      %{
        from: Address.to_text(x.from),
        id: Event.encode_id(x.id),
        reason: x.reason,
        time: x.time,
        to: Address.to_text(x.to),
        token: x.token,
        value: Utils.to_decimal(x.value, x.decimals)
      }
    end)
  end
end
