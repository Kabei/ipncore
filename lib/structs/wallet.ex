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
          true | :error
  def update_nonce(dets, tx, from, nonce) do
    DetsPlux.get_cache(dets, tx, from, 0)

    if DetsPlux.update_counter(tx, from, 1) == nonce do
      true
    else
      DetsPlux.update_counter(tx, from, -1)
      :error
    end
  end

  @spec update_nonce!(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          nil | no_return()
  def update_nonce!(dets, tx, from, nonce) do
    DetsPlux.get_cache(dets, tx, from, 0)

    if DetsPlux.update_counter(tx, from, 1) != nonce do
      DetsPlux.update_counter(tx, from, -1)
      raise IppanError, "Invalid nonce x3"
    end
  end

  @spec gte_nonce!(DetsPlux.db(), DetsPlux.transaction(), binary, integer()) ::
          nil | no_return()
  def gte_nonce!(dets, tx, from, nonce) do
    count = DetsPlux.get_cache(dets, tx, from, 0) + 1

    if count > nonce do
      raise IppanError, "Invalid nonce x4"
    end
  end

  def revert_nonce(tx, from) do
    DetsPlux.update_counter(tx, from, -1)
  end
end
