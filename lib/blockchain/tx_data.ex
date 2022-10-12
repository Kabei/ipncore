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

  @spec decode!(binary, String.t()) :: any()
  def decode!(data, "CBOR") do
    CBOR.decode(data)
    |> elem(1)
  end

  def decode!(data, "JSON") do
    Jason.decode!(data)
  end

  def decode!(data, _type), do: data

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
