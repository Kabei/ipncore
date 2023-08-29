defmodule Ippan.Validator do
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hostname: String.t(),
          name: String.t(),
          owner: binary(),
          pubkey: binary(),
          net_pubkey: binary(),
          avatar: String.t(),
          fee_type: integer(),
          fee: 0 | 1 | 2,
          stake: non_neg_integer(),
          failures: integer(),
          created_at: non_neg_integer(),
          updated_at: non_neg_integer()
        }

  defstruct [
    :id,
    :hostname,
    :name,
    :owner,
    :pubkey,
    :net_pubkey,
    :avatar,
    :fee_type,
    :fee,
    :stake,
    :failures,
    :created_at,
    :updated_at
  ]

  use Ippan.Struct
  def editable, do: ~w(hostname name avatar pubkey net_pubkey fee fee_type owner)

  def optionals, do: ~w(avatar)

  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.name,
      x.owner,
      x.pubkey,
      x.net_pubkey,
      x.avatar,
      x.fee_type,
      x.fee,
      x.stake,
      x.failures,
      x.created_at,
      x.updated_at
    ]
  end

  def to_tuple([id | _] = x) do
    {id, to_map(x)}
  end

  def to_tuple(x) do
    {x.id, x}
  end

  def to_map([
        id,
        hostname,
        name,
        owner,
        pubkey,
        net_pubkey,
        avatar,
        fee_type,
        fee,
        stake,
        failures,
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
      net_pubkey: net_pubkey,
      fee: fee,
      fee_type: fee_type,
      stake: stake,
      failures: failures,
      created_at: created_at,
      updated_at: updated_at
    }
  end
end
