defmodule Ippan.LocalNode do
  @behaviour Ippan.Struct

  @type t :: %__MODULE__{
          id: binary,
          hostname: charlist(),
          port: non_neg_integer(),
          role: [binary] | nil,
          pubkey: binary,
          net_pubkey: binary,
          avatar: binary | nil
        }

  defstruct [
    :id,
    :hostname,
    :port,
    :role,
    :pubkey,
    :net_pubkey,
    :avatar
  ]

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
      CBOR.encode(x.role),
      x.pubkey,
      x.net_pubkey,
      x.avatar
    ]
  end

  @impl true
  def list_to_tuple([id | _] = x) do
    {id, list_to_map(x)}
  end

  @impl true
  def to_tuple(x) do
    {x.id, x}
  end

  @impl true
  def list_to_map([
        id,
        hostname,
        port,
        role,
        pubkey,
        net_pubkey,
        avatar
      ]) do
    %{
      id: id,
      hostname: hostname,
      port: port,
      role: CBOR.decode(role) |> elem(1),
      pubkey: pubkey,
      net_pubkey: net_pubkey,
      avatar: avatar
    }
  end

  @impl true
  def to_map({_id, x}), do: x
end
