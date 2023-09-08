defmodule Ippan.Validator do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: non_neg_integer(),
          hostname: String.t(),
          port: integer(),
          name: String.t(),
          owner: binary(),
          pubkey: binary(),
          net_pubkey: binary(),
          avatar: String.t() | nil,
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
    :port,
    :name,
    :owner,
    :pubkey,
    :net_pubkey,
    :avatar,
    :fee_type,
    :fee,
    :stake,
    :created_at,
    :updated_at,
    failures: 0
  ]

  @impl true
  def editable, do: ~w(hostname port name avatar pubkey net_pubkey fee fee_type owner)
  @impl true
  def optionals, do: ~w(avatar)

  @impl true
  def to_list(x) do
    [
      x.id,
      x.hostname,
      x.port,
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
      port: port,
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

  @impl true
  def to_map({_id, x}) do
    x
  end
end
