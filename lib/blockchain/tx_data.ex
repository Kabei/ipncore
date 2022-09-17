defmodule Ipncore.TxData do
  use Ecto.Schema
  import Ecto.Query, only: [from: 2]
  alias Ipncore.Repo
  alias __MODULE__

  @primary_key {:txid, :binary, []}
  schema "txd" do
    field(:data, :binary)
    field(:mime, :string)
  end

  def get(txid) do
    from(txd in TxData, where: txd.txid == ^txid, select: {txd.mime, txd.data})
    |> Repo.one()
  end

  def get_data(txid, "json") do
    case get(txid) do
      {"CBOR", data} ->
        CBOR.decode(data)
        |> elem(1)
        |> Jason.encode!()

      {"JSON", data} ->
        data

      {"TEXT", data} ->
        data

      {_, data} ->
        data

      nil ->
        ""
    end
  end

  def multi_insert(multi, _name, _index, nil, _mime, _channel) do
    multi
  end

  def multi_insert(multi, _name, _index, "", _mime, _channel) do
    multi
  end

  def multi_insert(multi, name, index, bin_data, mime, channel) when byte_size(mime) in 1..5 do
    Ecto.Multi.insert(
      multi,
      name,
      %TxData{
        txid: index,
        data: bin_data,
        mime: mime
      },
      returning: false,
      prefix: channel
    )
  end
end
