defmodule Ippan.Account do
  # use DBTable, name: :account, path: "account", shards: 256, auto_save: 60_000,

  @doc """
  * `id`: Account name
  * `validator` - Validator assigned
  * `pubkey` - Falcon-512 public key
  * `pkhash` - is last pubkey-hash. Use to verify private key (seed)
  * `lhmac` - is last transaction mac-hash. Use to verify next transaction
  * `lhash` - last transaction hash
  """
  @type t :: %__MODULE__{
          id: String.t(),
          validator: non_neg_integer(),
          address: binary,
          auth_hash: binary,
          pubkey: binary,
          created_at: non_neg_integer()
        }

  @enforce_keys [:id, :validator, :address, :auth_hash, :pubkey]
  defstruct [:id, :validator, :address, :auth_hash, :pubkey, :created_at]

  def to_list(x) do
    [x.id, x.validator, x.address, x.auth_hash, x.pubkey, x.created_at]
  end

  def to_tuple(x) do
    {x.id, x.validator, x.address, x.auth_hash, x.pubkey, x.created_at}
  end

  def to_map({id, validator, address, auth_hash, pubkey, created_at}) do
    %{
      id: id,
      validator: validator,
      address: address,
      auth_hash: auth_hash,
      pubkey: pubkey,
      created_at: created_at
    }
  end

  def to_map([id, validator, address, auth_hash, pubkey, created_at]) do
    %{
      id: id,
      validator: validator,
      address: address,
      auth_hash: auth_hash,
      pubkey: pubkey,
      created_at: created_at
    }
  end
end
