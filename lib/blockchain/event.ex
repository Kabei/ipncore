defmodule Ipncore.Event do
  use Ecto.Schema
  alias Ipncore.{Chain, Repo}

  @primary_key {:id, :binary, []}
  schema "event" do
    field(:id, :binary)
    field(:hash, :binary)
    field(:type, :integer)
    field(:payload, :string)
    field(:sigs, {:array, :binary})
    field(:status, :integer, default: 100)
    field(:size, :integer, default: 0)
    field(:time, :integer)
  end

  def multi_insert(multi, name, event, status, time, channel) do
    Ecto.Multi.insert(multi, %{event: event, status: status, time: time},
      returning: false,
      prefix: channel
    )
  end
end
