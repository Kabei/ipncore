defmodule Ippan.Wallet do
  @behaviour Ippan.Struct
  @type t :: %__MODULE__{
          id: String.t(),
          pubkey: binary,
          validator: non_neg_integer(),
          created_at: non_neg_integer()
        }

  defstruct [:id, :pubkey, :validator, :created_at]

  @impl true
  def to_list(x) do
    [x.id, x.pubkey, x.validator, x.created_at]
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
  def to_map({id, pubkey, validator, created_at}) do
    %{
      id: id,
      pubkey: pubkey,
      validator: validator,
      created_at: created_at
    }
  end

  @impl true
  def list_to_map([id, pubkey, validator, created_at]) do
    %{
      id: id,
      pubkey: pubkey,
      validator: validator,
      created_at: created_at
    }
  end

  @spec update_nonce(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          pos_integer() | :error
  def update_nonce(dets, tx_name, from, nonce) do
    tx = DetsPlux.tx(tx_name)
    key = DetsPlux.tuple(from, "n")
    count = DetsPlux.get_tx(dets, tx, key, 0) + 1

    if count == nonce do
      DetsPlux.put(tx, key, count)
      count
    else
      :error
    end
  end

  @spec update_nonce!(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          pos_integer() | no_return()
  def update_nonce!(dets, tx_name, from, nonce) do
    tx = DetsPlux.tx(tx_name)
    key = DetsPlux.tuple(from, "n")
    count = DetsPlux.get_tx(dets, tx, key, 0) + 1

    if count != nonce do
      raise IppanError, "Invalid nonce"
    else
      DetsPlux.put(tx, key, count)
      count
    end
  end
end
