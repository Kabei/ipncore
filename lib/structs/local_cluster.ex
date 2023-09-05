defmodule LocalCluster do
  @behaviour Ippan.Struct

  @type t :: %__MODULE__{
          id: binary,
          hostname: charlist(),
          port: non_neg_integer(),
          name: binary | nil,
          role: binary | nil,
          pubkey: binary,
          net_pubkey: binary,
          avatar: binary | nil,
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  defstruct [
    :id,
    :hostname,
    :name,
    :role,
    :pubkey,
    :net_pubkey,
    :avatar,
    :created_at,
    :updated_at,
    port: 4848
  ]

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
      x.name,
      x.role,
      x.pubkey,
      x.net_pubkey,
      x.avatar,
      x.created_at,
      x.updated_at
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
        name,
        role,
        pubkey,
        net_pubkey,
        avatar,
        created_at,
        updated_at
      ]) do
    %{
      id: id,
      hostname: hostname,
      port: port,
      name: name,
      role: role,
      pubkey: pubkey,
      net_pubkey: net_pubkey,
      avatar: avatar,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  @impl true
  def to_map({_id, x}), do: x
end
