defmodule Ippan.Wallet do
  @type t :: %__MODULE__{
          id: String.t(),
          pubkey: binary,
          validator: non_neg_integer(),
          created_at: non_neg_integer()
        }

  defstruct [:id, :pubkey, :validator, :created_at]

  def to_list({id, pubkey, validator, created_at}) do
    [id, pubkey, validator, created_at]
  end

  def to_list(x) do
    [x.id, x.pubkey, x.validator, x.created_at]
  end

  def to_tuple([id, pubkey, validator, created_at]) do
    {id, pubkey, validator, created_at}
  end

  def to_tuple(x) do
    {x.id, x.pubkey, x.validator, x.created_at}
  end

  def to_map({id, pubkey, validator, created_at}) do
    %{
      id: id,
      pubkey: pubkey,
      validator: validator,
      created_at: created_at
    }
  end

  def to_map([id, pubkey, validator, created_at]) do
    %{
      id: id,
      pubkey: pubkey,
      validator: validator,
      created_at: created_at
    }
  end
end
