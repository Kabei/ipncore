defmodule Owner do
  use GlobalConst.DummyModule
end

defmodule Platform do
  alias Ipncore.{Address, Token, Wallet}

  @token Default.token()

  def start do
    Default.token()
    |> Token.fetch()
    |> put()
  end

  def put(%{id: @token, owner: address}) do
    addr = Owner.get(:address, nil)

    cond do
      addr == nil or addr != address ->
        pubkey = Wallet.get(address)

        GlobalConst.new(Owner, %{
          address: address,
          address58: Address.to_text(address),
          pubkey: pubkey
        })

        :ok

      true ->
        :none
    end
  end

  def put(_), do: :none

  def has_owner? do
    case Owner.get(:address, false) do
      false ->
        false

      _ ->
        true
    end
  end

  def pubkey, do: Owner.get(:pubkey, nil)
  def address, do: Owner.get(:address, nil)
  def address58, do: Owner.get(:address58, nil)

  def owner?(address) when is_nil(address), do: false
  def owner?(address), do: Owner.get(:address, nil) == address
end
