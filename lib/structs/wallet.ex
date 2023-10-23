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
          non_neg_integer() | :error
  def update_nonce(dets, tx, from, nonce) do
    count = DetsPlux.get_tx(dets, tx, from, 0) + 1

    if count == nonce do
      DetsPlux.put(tx, from, count)
      count
    else
      :error
    end
  end

  @spec update_nonce!(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          non_neg_integer() | :error
  def update_nonce!(dets, tx, from, nonce) do
    count = DetsPlux.get_tx(dets, tx, from, 0) + 1

    if count == nonce do
      DetsPlux.put(tx, from, count)
      count
    else
      raise IppanError, "Invalid nonce"
    end
  end

  @spec gte_nonce!(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          binary() | no_return()
  def gte_nonce!(dets, tx, from, nonce) do
    count = DetsPlux.get_tx(dets, tx, from, 0)

    if nonce <= count do
      raise IppanError, "Invalid nonce"
    end
  end
end
