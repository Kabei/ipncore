defmodule Ipncore.Txi do
  use Ecto.Schema
  alias Ipncore.{Token, Tx, Txo, Repo}

  import Ecto.Query
  import Ipnutils.Filters
  alias __MODULE__

  @type t :: %__MODULE__{
          oid: binary(),
          txid: binary(),
          key: binary()
        }

  @primary_key false
  schema "txi" do
    field(:oid, :binary)
    field(:txid, :binary)
    field(:key, :binary)
  end

  def decode_references(refs) do
    Enum.map(refs, &Base62.decode(&1))
  end

  @spec create([Txo.t()] | Txo.t()) :: [t] | t
  def create([]), do: []

  def create([o | rest]) do
    [
      create(o)
    ] ++ create(rest)
  end

  def create(o) do
    %{
      oid: o.id
    }
  end

  @spec valid?(t | [t]) :: boolean()
  def valid?(nil), do: false
  def valid?([]), do: true

  def valid?([input | rest]) do
    if input.oid >= 0, do: valid?(rest), else: false
  end

  def valid?(%Txi{} = input), do: input.oid >= 0

  def calc_size([]), do: 0

  def calc_size([i | rest]) do
    calc_size(i) + calc_size(rest)
  end

  def calc_size(txi) do
    byte_size(txi.oid) + byte_size(txi.txid)
  end

  def calc_size(txis, index_size) do
    Enum.reduce(txis, 0, fn x, acc ->
      acc + byte_size(x.oid) + index_size
    end)
  end

  def all(params) do
    from(Txi)
    |> join(:inner, [txi], o in Txo, on: o.id == txi.oid)
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

    sub = from(tx in Tx, where: tx.hash == ^hash, select: tx.index)

    where(query, [txi], txi.txid == subquery(sub))
  end

  defp filter_index(query, %{"txid" => txid}) do
    where(query, [txi], txi.txid == ^Base62.decode(txid))
  end

  defp filter_index(query, _params), do: query

  defp filter_address(query, %{"address" => address}) do
    bin_address = Base58Check.decode(address)
    where(query, [_txi, o], o.address == ^bin_address)
  end

  defp filter_address(query, _params), do: query

  defp filter_token(query, %{"token" => token}) do
    where(query, [_txi, o], o.tid == ^token)
  end

  defp filter_token(query, _params), do: query

  defp filter_select(query, %{"show" => "token_meta"}) do
    query
    |> join(:inner, [_, txo], tk in Token, on: tk.id == txo.tid)
    |> select([txi, o, tk], %{
      id: txi.oid,
      token: o.tid,
      value: o.value,
      address: o.address,
      meta: tk.meta
    })
  end

  defp filter_select(query, %{"format" => "array"}) do
    select(query, [txi, o], [
      txi.oid,
      o.address,
      o.tid,
      o.value
    ])
  end

  defp filter_select(query, _params) do
    select(query, [txi, o], %{
      id: txi.oid,
      token: o.tid,
      value: o.value,
      address: o.address
    })
  end

  defp sort(query, params) do
    case Map.get(params, "sort") do
      "newest" ->
        order_by(query, [txi], desc: txi.oid)

      _ ->
        order_by(query, [txi], asc: txi.oid)
    end
  end

  defp transform(txis, %{"format" => "array"}) do
    Enum.map(txis, fn [id, address, token, value] ->
      [Base62.encode(id), Base58Check.encode(address), token, value]
    end)
  end

  defp transform(txis, _) do
    Enum.map(txis, fn x ->
      %{x | id: Base62.encode(x.id), address: Base58Check.encode(x.address)}
    end)
  end
end
