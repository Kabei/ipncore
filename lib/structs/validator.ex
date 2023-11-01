defmodule Ippan.Validator do
  require BigNumber
  alias Ippan.Utils
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
          fa: integer(),
          fb: integer(),
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
    :fa,
    :fb,
    :stake,
    :created_at,
    :updated_at,
    failures: 0
  ]

  @impl true
  def editable, do: ~w(hostname port name avatar pubkey net_pubkey fa fb owner)
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
      BigNumber.to_bin(x.fa),
      BigNumber.to_bin(x.fb),
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
        fa,
        fb,
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
      fb: BigNumber.to_int(fb),
      fa: BigNumber.to_int(fa),
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

  def to_text(x = %{pubkey: pk, net_pubkey: npk}) do
    %{x | pubkey: Utils.encode64(pk), net_pubkey: Utils.encode64(npk)}
  end

  def calc_price(next_id), do: (next_id + 1) * EnvStore.validator_price()

  require Sqlite

  defmacro insert(args) do
    quote location: :keep do
      Sqlite.step("insert_validator", unquote(args))
    end
  end

  defmacro get(id) do
    quote location: :keep do
      Sqlite.get(:validator, "get_validator", unquote(id), Ippan.Validator)
    end
  end

  defmacro next_id do
    quote location: :keep do
      Sqlite.one("next_id_validator")
    end
  end

  defmacro exists?(id) do
    quote location: :keep do
      Sqlite.exists?("exists_validator", [unquote(id)])
    end
  end

  defmacro exists_host?(hostname) do
    quote location: :keep do
      Sqlite.exists?("exists_host_validator", [unquote(hostname)])
    end
  end

  defmacro owner?(id, owner) do
    quote bind_quoted: [id: id, owner: owner], location: :keep do
      Sqlite.exists?("owner_validator", [id, owner])
    end
  end

  defmacro total do
    quote location: :keep do
      Sqlite.one("total_validators", [], 0)
    end
  end

  defmacro delete(id) do
    quote bind_quoted: [id: id], location: :keep do
      :ets.delete(:validator, id)
      Sqlite.step("delete_validator", [id])
    end
  end

  defmacro update(map, id) do
    quote location: :keep do
      :ets.delete(:validator, id)
      Sqlite.update("blockchain.validator", unquote(map), id: unquote(id))
    end
  end
end
