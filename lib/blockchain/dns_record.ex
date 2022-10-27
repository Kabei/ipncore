defmodule DnsRecord do
  alias __MODULE__
  use Ecto.Schema
  import Ecto.Query
  alias Ipncore.Repo

  @delay_edit Application.get_env(:ipncore, :tx_delay_edit)
  @fields ~w(name address email avatar)
  @edit_fields ~w(address enabled email avatar)

  schema "dns_record" do
    field(:name, :string)
    field(:type, :string)
    field(:value, :string)
    field(:ttl, :integer, default: 600)
  end

  def new(%{"name" => name, "type" => type, "value" => value} = params) do

  end
end
