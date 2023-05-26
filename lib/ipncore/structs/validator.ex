defmodule Ippan.Validator do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hostname: String.t(),
          name: String.t(),
          owner: binary(),
          avatar: String.t(),
          pubkey: binary(),
          fee_type: integer(),
          fee: 0 | 1 | 2,
          enabled: boolean(),
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  defstruct [
    :id,
    :hostname,
    :name,
    :owner,
    :avatar,
    :pubkey,
    :fee_type,
    :fee,
    :created_at,
    :updated_at,
    enabled: true
  ]

  use Ippan.Struct
  def editable, do: ~w(hostname name avatar pubkey fee fee_type)

  def optionals, do: ~w(avatar)

  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.name,
      x.owner,
      x.pubkey,
      x.avatar,
      x.fee_type,
      x.fee,
      x.enabled,
      x.created_at,
      x.updated_at
    ]
  end

  def to_tuple([
        id,
        hostname,
        name,
        owner,
        avatar,
        pubkey,
        fee_type,
        fee,
        enabled,
        created_at,
        updated_at
      ]) do
    {id, hostname, name, owner, pubkey, avatar, fee_type, fee, enabled, created_at, updated_at}
  end

  def to_tuple(x) do
    {x.id, x.hostname, x.name, x.owner, x.pubkey, x.avatar, x.fee_type, x.fee, x.enabled,
     x.created_at, x.updated_at}
  end

  def to_map(
        {id, hostname, name, owner, avatar, pubkey, fee_type, fee, enabled, created_at,
         updated_at}
      ) do
    %{
      id: id,
      hostname: hostname,
      name: name,
      owner: owner,
      avatar: avatar,
      pubkey: pubkey,
      fee: fee,
      fee_type: fee_type,
      enabled: enabled,
      created_at: created_at,
      updated_at: updated_at
    }
  end

  def to_map([
        id,
        hostname,
        name,
        owner,
        avatar,
        pubkey,
        fee_type,
        fee,
        enabled,
        created_at,
        updated_at
      ]) do
    %{
      id: id,
      hostname: hostname,
      name: name,
      owner: owner,
      avatar: avatar,
      pubkey: pubkey,
      fee: fee,
      fee_type: fee_type,
      enabled: enabled,
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
